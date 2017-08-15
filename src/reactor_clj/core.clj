(ns reactor-clj.core
  (:use [clojure.set]
        [clojure.walk]
        [reactor-clj.type-check :refer :all]))

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

(defn process-function-properties 
  [form [SMTLIB-commands type-decls]]
  (reduce (fn [[SMTLIB-commands type-decls] form]
            (if (= (first form) 'fn)
              (let [name (second form)
                    args (rest (rest form))]
                [(conj SMTLIB-commands )])))))

;; complete from here

(defn process-reactor [form type-decls]
  (let [name             (second form)
        decls            (rest (rest form))
        [input decls]    (get-section :input   decls)
        [output private] (get-section :output  decls)
        decls            (extract-decls input output private)]
    (println "decls:" decls)
    (println "meta decls:" (meta (first decls)))
    (println "found inputs:")
    (doseq [e input]
      (println "  " e))
    (println "found outputs:")
    (doseq [e output]
      (println "  " e))
    (println "found private declarations:")
    (doseq [e private]
      (println "  " e (extract-dependencies decls e)))))

(defn verify-reactor-file [filename]
  (let [old-ns-name (ns-name *ns*)]
    (in-ns 'reactor-file-private)
    (clojure.core/refer 'clojure.core)
    (reduce 
     (fn [type-decls]
        (println "working with form" form)
        (let [form (clojure.walk/macroexpand-all form)] 
          (println "It expanded to" form)
          (condp = (first form)
            'function-properties (process-function-properties type-decls)
            'reactor             (process-reactor form type-decls)
            type-decls)))
     type-decls
     [form (-> filename
                     (slurp)
                     (#(str "[" %1 "]"))
                     (read-string))])
    (in-ns old-ns-name)))

(verify-reactor-file "test/volume-1.rct")

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


