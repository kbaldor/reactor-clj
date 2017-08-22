(ns reactor-clj.core
  (:use [clojure.set :refer [intersection]]
        [clojure.walk]
        [reactor-clj.type-check :refer :all]
        [clojure.core.match :refer [match]]
        [clojure.pprint :refer [pprint]]
        [clojure.string :refer [join]]))

(def type-decls 
  (let [math-types       '(compound ((int int) int)
                                    ((int float) float)
                                    ((float int) float)
                                    ((float float) float))
        comparison-types '(compound ((int int) bool)
                                    ((int float) bool)
                                    ((float int) bool)
                                    ((float float) bool))
        function         #(list 'function %1)]
    {'+        math-types
     '-        math-types
     '*        math-types
     '/        math-types
     '<        comparison-types
     '>        comparison-types
     '<=       comparison-types 
     '>=       comparison-types 
     'filter   (function #(if (= 'stream (first %2)) %2 'error))
     'snapshot (function #(if (not= 'stream (first %2)) 
                            'error
                            (if (reactive-type? %1)
                              (list 'stream (second %1))
                              'error)))
     }))


;; This extracts the contents of sections that start
;; with the specified 'key' and contain an even number of
;; elements. For example, for decls
;;           ((section-name key1 value1
;;                          key2 value2))
;; calling get-section with a key of section-name
;; will yield ((key1 value1) (key2 value2))

(defn- get-section [key decls]
  (let [groups (group-by #(= key (first %)) decls)]
    [(->> (groups true)
          (map rest)
          first
          (partition 2))
     (groups false)]))

(defn- extract-dependencies [decls expr]
  (intersection (into #{} decls) (into #{} (flatten expr))))

(defn- extract-decls [& groups]
  (apply concat (for [group groups] (map first group))))

(defn SMTLIB-type [reactor-type]
  (match reactor-type
         'int 'Int))

(defn SMTLIB-var-type [var-reactor-type]
  (let [[var reactor-type] var-reactor-type]
    (list var (SMTLIB-type reactor-type))))

(defn format-assertion [assertion]
  (match (into []  assertion)
         ['forall vars statement]
         (list 'forall (map SMTLIB-var-type (partition 2 vars)) 
               (format-assertion statement))
         :else assertion))

(defn update-SMTLIB-commands [commands statement]
  (match (into [] statement)
         ['fn name parameter-types return-type]
         (conj commands (list 'declare-fun name
                              (map SMTLIB-type parameter-types)
                              (SMTLIB-type return-type)))
         ['forall & rest] 
         (conj commands (list 'assert (format-assertion statement)))
         :else commands))

(defn update-type-decls [type-decls statement]
  (match (into [] statement)
         ['fn name parameter-types return-type]
         (assoc type-decls name (list 'simple parameter-types return-type))
         :else type-decls))

(defn process-function-property [[commands type-decls] statement]
  [(update-SMTLIB-commands commands statement)
   (update-type-decls type-decls statement)]) 

;; Takes a function-properties block and a pair
;; containing the SMTLIB commands and the type
;; declarations found so far and returns a new
;; set of SMTLIB commands and type declarations.

(defn process-function-properties 
  [[commands type-decls] function-properties-block]
  (reduce process-function-property 
          [commands type-decls] 
          (rest function-properties-block)))

(defn sub-divisible? [statement]
  (if (sequential? statement) 
    (match (into [] statement)
           ['fn* & rest] false
           ['fn  & rest] false
           :else true)
    false))

(defn get-subexpressions [name expression]
  (let [[expression index children]
        (reduce (fn [[expression index children] child]
                  (if (sub-divisible? child)
                    (let [sub-name (symbol (str name "-" index))] 
                      [(conj expression sub-name)
                       (inc index)
                       (conj children [sub-name child])])
                    [(conj expression child)
                     index
                     children])) [[(first expression)] 1 []] (rest expression))]
    (reduce into [[name (apply list expression)]] 
            (map (partial apply get-subexpressions) children))))

;(get-subexpressions 'X '(filter (fn [x] (> x 5)) (+ 1 (- 2 3) 4 (* 5 (/ 6 7)))))

(defn process-reactor [[commands type-decls] form]
  (let [name                  (second form)
        decls                 (rest (rest form))
        [input         decls] (get-section :input   decls)
        [output        decls] (get-section :output  decls)
        [assumes       decls] (get-section :assumes  decls)
        [gurarantees private] (get-section :guarantees  decls)
        decls                 (extract-decls input output private)]
    (println private)
    (doseq [[name expr] private]
      (println name)
      (doseq [[name expr] (get-subexpressions name expr)]
        (println name expr)))
    (println "found inputs:")
    (doseq [e input]
      (println "  " e))
    (println "found outputs:")
    (doseq [e output]
      (println "  " e))
    (println "found private declarations:")
    (doseq [e private]
      (println "  " e (extract-dependencies decls e))))
  [commands type-decls])

(verify-reactor-file "test/volume-1.rct" {})

(defn process-form [[commands type-decls] form]
  (println "working with form")
  (let [form form #_(clojure.walk/macroexpand-all form)] 
    (println "It expanded to" form)
    (condp = (first form)
      'function-properties (process-function-properties [commands type-decls] form)
      'reactor             (process-reactor [commands type-decls] form)
      [commands type-decls])))

(defn verify-reactor-file [filename type-decls]
  (let [old-ns-name (ns-name *ns*)]
    (in-ns 'reactor-file-private)
    (clojure.core/refer 'clojure.core)
    (let [[commands type-decls] (reduce process-form [[] type-decls]
                                        (-> filename
                                            (slurp)
                                            (#(str "[" %1 "]"))
                                            (read-string)))]
      (pprint commands)
      (pprint type-decls)
      (spit (join [filename ".smt"]) (join "\n" (map str commands))))
    (in-ns old-ns-name)))

(verify-reactor-file "test/volume-1.rct" {})

(macroexpand '(unknown (reactor
           (input       Vmin       int
                        Vmax       int
                        Vstep      int
                        increase  *unit
                        decrease  *unit)
           (output      V         (hold changes Vmin))

           (increases (filter #(<= %1 Vmax) 
                              (snapshot increase (+ V' Vstep))))
           (decreases (filter #(>= %1 Vmin) 
                              (snapshot decrease (- V' Vstep))))
           (changes   (or-else increases decreases)))))

(macroexpand (read-string "(reactor (+ 1 2 3 4 5 6 7 8 9))"))
(meta (read-string "(reactor (+ 1 2 3 4 5 6 7 8 9))"))


