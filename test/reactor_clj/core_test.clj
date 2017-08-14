(ns reactor-clj.core-test
  (:require [clojure.test :refer :all]
            [reactor-clj.core :refer :all]))

(def type-decls {'min '((int int) int)
                 'max '((int int) int)
                 'X   'float
                 'Y   '(stream float)})

#_(get-base-type '(filter #(< %1 10) Y) type-decls)


(deftest cell-base-type-test
  (testing "cell base type"
    (is (= 'int (get-base-type '(cell int) type-decls)))))

(deftest stream-base-type-test
  (testing "stream base type"
    (is (= 'int (get-base-type '(stream int) type-decls)))))

(deftest filter-stream-type-test
  (testing "filter stream type"
    (is (= '(stream float) (get-type '(filter #(true) Y) type-decls)))))

(get-type '(filter #(true) Y) type-decls)
