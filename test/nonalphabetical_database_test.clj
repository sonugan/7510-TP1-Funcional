(ns nonalphabetical-database-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def nonalphabetical-database "
	Var_on(juan).
	Var_on(pepe).
	Var_on(hector).
	Var_on(roberto).
	Var_on(alejandro).
	m.uj$r(maria).
	m.uj$r(cecilia).
	pa dre(juan, pepe).
	pa dre(juan, pepa).
	pa dre(hector, maria).
	pa dre(roberto, alejandro).
	pa dre(roberto, cecilia).
	hijo        (X, Y) :- Var_on(X), pa dre(Y, X).
	hi:-ja(X, Y) :- m.uj$r(X), pa dre(Y, X).
")

(deftest nonalphabetical-database-fact-test
  (testing "Var_on(juan) should be true"
    (is (= (evaluate-query nonalphabetical-database "Var_on(juan)")
           true)))
  (testing "Var_on(maria) should be false"
    (is (= (evaluate-query nonalphabetical-database "Var_on(maria)")
           false)))
  (testing "m.uj$r(cecilia) should be true"
    (is (= (evaluate-query nonalphabetical-database "m.uj$r(cecilia)")
           true)))
  (testing "pa dre(juan, pepe) should be true"
    (is (= (evaluate-query nonalphabetical-database "pa dre(juan, pepe)")
           true)))
  (testing "pa dre(mario, pepe) should be false"
    (is (= (evaluate-query nonalphabetical-database "pa dre(mario, pepe)")
           false))))

(deftest nonalphabetical-database-rule-test
  (testing "hijo(pepe, juan) should be true"
    (is (= (evaluate-query nonalphabetical-database "hijo(pepe, juan)")
           true)))
  (testing "hi:-ja(maria, roberto) should be nil, ':-' is an special string for Rule definition"
    (is (= (evaluate-query nonalphabetical-database "hi:-ja(maria, roberto)")
           nil))))

(deftest nonalphabetical-database-empty-query-test
  (testing "Var_on should be nil"
    (is (= (evaluate-query nonalphabetical-database "Var_on")
           nil)))
  (testing "maria should be nil"
    (is (= (evaluate-query nonalphabetical-database "maria")
           nil)))
  (testing "empty should be nil"
    (is (= (evaluate-query nonalphabetical-database "")
           nil))))

(deftest nonalphabetical-database-wrong-parameters-query-test
  (testing "hijo(pepe, juan) should be true"
    (is (= (evaluate-query nonalphabetical-database "hijo    (    pepe  , juan    )")
           true)))
  (testing "Var_on(juan) should be true"
    (is (= (evaluate-query nonalphabetical-database "Var_on(    juan    )")
           true))))

(deftest nonalphabetical-database-blank-spaces-in-query-test
  (testing "hijo has two parameters should be nil"
    (is (= (evaluate-query nonalphabetical-database "hijo(maria,pepe,juan)")
           nil)))
  (testing "Var_on has one parameter should be nil"
    (is (= (evaluate-query nonalphabetical-database "Var_on()")
           nil))))          
