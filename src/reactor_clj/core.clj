(ns reactor-clj.core)

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

(defmacro reactor [& decls]
  (let [[input decls2]   (get-section 'input   decls)
        [output decls3]  (get-section 'output  decls2)
        private         decls3]
    (println "found inputs" input)
    (println "found outputs" output)
    (println "found private declarations" private)))

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
 (changes (merge increments decrements #(+ %1 %2)))
 (proposed-volume (+ volume changes))
 (legal-changes (restrict proposed-volume min-volume max-volume))
 (output (volume (hold legal-changes min-volume))))
