(ns reactor-clj.core
  (:use [clojure.set]))

;; This extracts the contents of sections that start
;; with the specified 'key'. For example,
;; for decls ((section-name item1
;;                          (item2 item3)))
;; calling get-section with a key of section-name
;; will yield (item1 (item2 item3)).
;; It also supports 
(defn- get-section [key decls]
  (let [groups (group-by #(= key (first %)) decls)]
    [(->> (groups true)
          (map rest)
          (reduce concat))
     (groups false)]))

(defn- extract-dependencies [defns expr]
  (intersection (into #{} defns) (into #{} (flatten expr))))

(defn- extract-defns [& groups]
  (apply concat (for [group groups] (map first group))))

(defmacro reactor [& decls]
  (let [[input decls]    (get-section 'input   decls)
        [output private] (get-section 'output  decls)
         decls           (extract-defns input output private)]
    (println decls)
    (println "found inputs:")
    (doseq [e input]
      (println "  " e))
    (println "found outputs:")
    (doseq [e output]
      (println "  " e))
    (println "found private declarations:")
    (doseq [e private]
      (println "  " e (extract-dependencies defns e)))))

(defn restrict [input min-legal max-legal]
  (max min-legal (min max-legal input)))

(reactor
 (input (min-volume  int)
        (max-volume  int)
        (step-size   int)
        (increment  *unit)
        (decrement  *unit)
        (set-value  *int))
 (increments (snapshot increment step-size))
 (decrements (snapshot decrement (- step-size)))
 #_(changes (merge increments decrements #(+ %1 %2)))
 (changes (merge increments decrements (fn [a b] (+ a b))))
 (proposed-volume (+ volume changes))
 (legal-changes (restrict proposed-volume min-volume max-volume))
 (output (volume (hold legal-changes min-volume))))
