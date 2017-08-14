(ns reactor-clj.type-check-test
  (:require [clojure.test :refer :all]
            [reactor-clj.type-check :refer :all]))

(defn math-op-type [& arg-types]
  (let [arg-types (map get-base-type arg-types)]
    (cond 
     (contains? arg-types 'float) 'float
     (contains? arg-types 'int)   'int
     :else                        'error)))

(def type-decls {'min '(simple (int int) int)
                 'max '(simple (int int) int)
                 'X   '(simple float)
                 'Y   '(simple int)
                 'Z   '(simple (stream float))
                 '+   '(compound ((int int) int)
                                 ((int float) float)
                                 ((float int) float)
                                 ((float float) float))
                 'filter (list 'function #(if (= 'stream (first %2))
                                            %2
                                            'error))
                 'snapshot (list 'function 
                                 #(if (not= 'stream (first %2)) 
                                    'error
                                    (if (reactive-type? %1)
                                      (list 'stream (second %1))
                                      'error)))
                 'A '(simple (cell float))
                 'B '(simple (stream float))
                 'C '(simple (cell int))
                 'D '(simple (stream int))
                 'E '(simple (cell int))
                 })




(deftest simple-float-test
  (testing "simple float"
    (is (= 'float (get-type 'X type-decls)))))

(deftest simple-int-test
  (testing "simple int"
    (is (= 'int (get-type 'Y type-decls)))))

(deftest simple-math-test
  (testing "simple math"
    (is (= 'float (get-type '(+ X Y) type-decls)))))

(deftest filter-test
  (testing "filter"
    (is (= '(stream float) (get-type '(filter #(true) Z) type-decls)))))

(deftest snapshot-test
  (testing "snapshot"
    (is (= '(stream float) (get-type '(snapshot (+ A C) Z) type-decls)))))

(deftest lift-test-1
  (testing "float cell lift")
  (is (= '(cell float) (get-type '(+ A C) type-decls))))

(deftest lift-test-2
  (testing "int cell lift")
  (is (= '(cell int) (get-type '(+ C E) type-decls))))

(deftest lift-test-3
  (testing "illegal double stream lift")
  (is (= 'error (get-type '(+ B D) type-decls))))

