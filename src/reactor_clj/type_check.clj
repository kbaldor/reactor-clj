(ns reactor-clj.type-check
  (:use [clojure.set]
        [clojure.walk]))

;; Type declarations are a hashmap from symbols
;; to type signatures. Type signatures must be
;; of one of four forms:
;;
;; (simple              <type>)
;; (simple    (<type>+) <type>)
;; (compound ((<type>+) <type>)
;;           ((<type>+) <type>)
;;            ...              )
;; (function f) : f in (<type>+) -> <type>

(defn match-type-decl [type-decl arg-types]
  ;(println "matching-type-decl" type-decl "with" arg-types)
  (condp = (first type-decl)
    'simple (last type-decl)
    'function (apply (second type-decl) arg-types)
    'compound (let [matches (filter #(= arg-types (first %1)) (rest type-decl))]
                (if (empty? matches)
                  'unknown
                  (second (first matches))))))

(defn stream-type? [type-id]
  (and (sequential? type-id) (= 'stream (first type-id))))

(defn cell-type? [type-id]
  (and (sequential? type-id) (= 'cell (first type-id))))

(defn reactive-type? [type-id]
  (or (stream-type? type-id) (cell-type? type-id)))

(defn singleton? [collection] (= 1 (count collection)))

(defn get-base-type [type-id]
  ;(println "getting base type for" type-id)
  (if (reactive-type? type-id)
    (second type-id)
    type-id))


;; NOTE that one stream-type input is supported, but _only_ one
;; Also, any mixture of cells and non-reactive types is treated
;; the same as all cells.
;; This is sufficient for the SMT conversion, but may have to
;; be revisited for the Sodium conversion.
(defn get-lifted-type [type-decl arg-types]
  ;(println "getting lifted type for" type-decl arg-types)
  (let [base-type (match-type-decl type-decl (map get-base-type arg-types))]
   (cond
    (= 'unknown base-type) 'unknown
    (singleton? (filter stream-type? arg-types)) (list 'stream base-type)
    (not-empty  (filter cell-type? arg-types)) (list 'cell base-type)
    :else 'error)))

(defn get-type [expr type-decls]
  ;(println "getting type for" expr)
  (if (sequential? expr)
    (let [func-name (first expr)]
      (if (contains? type-decls func-name)
       (let [arg-types (map #(get-type %1 type-decls) (rest expr))
             func-type (type-decls func-name)
             initial-type (match-type-decl func-type arg-types)]
         (if (= 'unknown initial-type)
           (get-lifted-type func-type arg-types)
           initial-type))
       'unknown))
    (if (contains? type-decls expr)
      (match-type-decl (type-decls expr) '()))))


