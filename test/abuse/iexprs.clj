(ns abuse.iexprs
  (:require clojure.test))

(deftest one-form
  (is (=
    #I
      :foo
    :foo)))

(deftest one-line
  (is (=
    #I
      vec :foo :bar
    (vec :foo :bar))))

(deftest immediate-termination #I)