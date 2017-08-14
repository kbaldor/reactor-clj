(ns reactor-clj.core
  (:use [clojure.set]
        [clojure.walk]))

(def third (comp second rest))

(defn get-return-type [type-decl]
  (if (sequential? type-decl)
    (last type-decl)
    type-decl))

;; This returns the type of a reactive or
;; standard expression. It currently only supports
;; simply-typed function declarations

(defn get-type [expr type-decls]
  #_(println "getting type for" expr)
  (cond 
   (contains? type-decls expr)         (get-return-type (type-decls expr))
   (contains? type-decls (first expr)) (get-return-type (type-decls (first expr)))
   :else      (condp = (first expr)
                'cell   expr
                'stream expr
                'filter (list 'stream (get-type (third expr) type-decls)))))

(defn get-base-type [reactive-type type-decls]
  #_(println "Getting base type for" reactive-type)
  (condp = (first reactive-type)
    'cell (second reactive-type)
    'stream (second reactive-type)
    (get-base-type (get-type reactive-type type-decls) type-decls)))


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

(defn process-reactor [form]
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

(defn read-from-file-with-trusted-contents [filename]
  (with-open [r (java.io.PushbackReader.
                 (clojure.java.io/reader filename))]
    (binding [*read-eval* true]
      (read r))))

(defn verify-reactor-file [filename]
  (let [old-ns-name (ns-name *ns*)]
    (in-ns 'reactor-file-private)
    (clojure.core/refer 'clojure.core)
    (doseq [form (-> filename
                     (slurp)
                     (#(str "[" %1 "]"))
                     (read-string))]
      (do
        (println "working with form" form)
        (let [form (clojure.walk/macroexpand-all form)] 
          (println "It expanded to" form)
          (condp = (first form)
            'function-properties (println "found function properties")
            'reactor             (process-reactor form)
            (println "unhandled (for now)")
            #_(println "eval returned" (eval form))))))
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


