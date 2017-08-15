(ns reactor-clj.core-test
  (:require [clojure.test :refer :all]
            [reactor-clj.core :refer :all]
            [reactor-clj.type-check :refer :all]))

(def test-type-decls 
  (merge type-decls
         {'A '(simple int)
          'B '(simple int)
          'C '(simple int)
          'D '(simple int)
          }))

(deftest math-type-test
  (testing "math expr type"
    (is (= 'int (get-type '(* (+ A B) (- C D)) test-type-decls)))))

(get-type '(* (+ A B) (- C D)) test-type-decls)
