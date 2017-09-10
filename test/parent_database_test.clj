(ns parent-database-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def parent-database "
	varon(juan).
	varon(pepe).
	varon(hector).
	varon(roberto).
	varon(alejandro).
	mujer(maria).
	mujer(cecilia).
	padre(juan, pepe).
	padre(juan, pepa).
	padre(hector, maria).
	padre(roberto, alejandro).
	padre(roberto, cecilia).
	hijo(X, Y) :- varon(X), padre(Y, X).
	hija(X, Y) :- mujer(X), padre(Y, X).
")

(deftest parent-database-fact-test
  (testing "varon(juan) should be true"
    (is (= (evaluate-query parent-database "varon(juan)")
           true)))
  (testing "varon(maria) should be false"
    (is (= (evaluate-query parent-database "varon(maria)")
           false)))
  (testing "mujer(cecilia) should be true"
    (is (= (evaluate-query parent-database "mujer(cecilia)")
           true)))
  (testing "padre(juan, pepe) should be true"
    (is (= (evaluate-query parent-database "padre(juan, pepe)")
           true)))
  (testing "padre(mario, pepe) should be false"
    (is (= (evaluate-query parent-database "padre(mario, pepe)")
           false))))

(deftest parent-database-rule-test
  (testing "hijo(pepe, juan) should be true"
    (is (= (evaluate-query parent-database "hijo(pepe, juan)")
           true)))
  (testing "hija(maria, roberto) should be false"
    (is (= (evaluate-query parent-database "hija(maria, roberto)")
           false))))

(deftest parent-database-empty-query-test
  (testing "varon should be nil"
    (is (= (evaluate-query parent-database "varon")
           nil)))
  (testing "maria should be nil"
    (is (= (evaluate-query parent-database "maria")
           nil)))
  (testing "empty should be nil"
    (is (= (evaluate-query parent-database "")
           nil))))

(deftest parent-database-wrong-parameters-query-test
  (testing "hijo has two parameters not three should be nil"
    (is (= (evaluate-query parent-database "hijo(maria,pepe,juan)")
           nil)))
  (testing "hijo has two parameters not zeroshould be nil"
    (is (= (evaluate-query parent-database "hijo()")
           nil)))
  (testing "varon has one parameter should be nil"
    (is (= (evaluate-query parent-database "varon()")
           nil))))
