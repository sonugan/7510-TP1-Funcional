(ns nonalphabetical-database-test
  (:require [clojure.test :refer :all]
            [logical-interpreter :refer :all]))

(def nonalphabetical-database "
	Var_on(juan_1).
	Var_on(pepe{23).
	Var_on(hector).
	Var_on(roberto).
	Var_on(alejandro).
	m.uj$r(maria).
	m.uj$r(cecilia).
	padre(juan_1, pepe{23).
	padre(juan_1, pepa).
	padre(hector, maria).
	padre(roberto, alejandro).
	padre(roberto, cecilia).
	hijo        (XX_1-, Y) :- Var_on(XX_1-), padre(Y, XX_1-).
	hi:-ja(XX, Y) :- m.uj$r(XX), padre(Y, XX).
")

(deftest nonalphabetical-database-fact-test
  (testing "Var_on(juan_1) should be true"
    (is (= (evaluate-query nonalphabetical-database "Var_on(juan_1)")
           true)))
  (testing "Var_on(maria) should be false"
    (is (= (evaluate-query nonalphabetical-database "Var_on(maria)")
           false)))
  (testing "m.uj$r(cecilia) should be true"
    (is (= (evaluate-query nonalphabetical-database "m.uj$r(cecilia)")
           true)))
  (testing "padre(juan_1, pepe{23) should be true"
    (is (= (evaluate-query nonalphabetical-database "padre(juan_1, pepe{23)")
           true)))
  (testing "padre(mario, pepe{23) should be false"
    (is (= (evaluate-query nonalphabetical-database "padre(mario, pepe{23)")
           false))))

(deftest nonalphabetical-database-rule-test
  (testing "hijo(pepe{23, juan_1) should be true"
    (is (= (evaluate-query nonalphabetical-database "hijo(pepe{23, juan_1)")
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
  (testing "hijo(pepe{23, juan_1) should be true"
    (is (= (evaluate-query nonalphabetical-database "hijo    (    pepe{23  , juan_1    )")
           true)))
  (testing "Var_on(juan_1) should be true"
    (is (= (evaluate-query nonalphabetical-database "Var_on(    juan_1    )")
           true))))

(deftest nonalphabetical-database-blank-spaces-in-query-test
  (testing "hijo has two parameters should be nil"
    (is (= (evaluate-query nonalphabetical-database "hijo(maria,pepe{23,juan_1)")
           nil)))
  (testing "Var_on has one parameter should be nil"
    (is (= (evaluate-query nonalphabetical-database "Var_on()")
           nil))))                            
