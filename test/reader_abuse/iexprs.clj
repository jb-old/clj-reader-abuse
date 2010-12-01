(ns reader-abuse.iexprs
  (:use reader-abuse.iexprs)
  (:use clojure.test)
  (:import [clojure.lang LineNumberingPushbackReader])
  (:import [java.io StringReader]))

(defn input
  [s] (LineNumberingPushbackReader. (StringReader. s)))

; I'm not comfortable with the susinct notations used for macros.
(defmacro is=
  [& forms] (list 'is (list* '= forms)))

; read-hws, those tests are easy to write.
; oh good, I have figured out how to write a test
(deftest read-hws-null
  (is= 0 (read-hws (input ""))))

(deftest read-hws-some
  (let [in (input "  ")]
    (is= 2 (read-hws in))
    (is (#{-1 65535} (.read in))))); eof odditude

(deftest read-hws-nonws-terminated
  (let [in (input "   a ")]
    (is= 3 (read-hws in))
    (is= (int \a) (.read in))))

(deftest read-hws-newline-terminated
  (let [in (input " \n  ")]
    (is= 1 (read-hws in))
    (is= (int \newline) (.read in))))

(deftest read-hws-starting-with-newline
  (let [in (input "\n  ")]
    (is= 0 (read-hws in))
    (is= (int \newline) (.read in))))

(deftest read-hws-starting-with-nonws
  (let [in (input "a ")]
    (is= 0 (read-hws in))
    (is= (int \a) (.read in))))

; read-iexprs
(deftest read-iexpers-single-delimiter-terminated
  (is= '(do 4) (read-iexprs (input "\n4)"))))

(deftest read-iexprs-single-eof-terminated
  (is= '(do 3) (read-iexprs (input "\n3"))))

(deftest read-iexprs-single-dedent-terminated
  (is= '(do 5) (read-iexprs (input "\n\t5\n\nfoo"))))

(deftest read-iexprs-single-custom-wrapper-eof
  (is= '(foo 3) (read-iexprs (input "foo\n3"))))

(deftest read-iexprs-line
  (is= '(do (a b c)) (read-iexprs (input "\na b c"))))

(deftest read-iexprs-multiple-simple
  (is= '(do 4 5 6) (read-iexprs (input "\n4\n5\n6"))))

(deftest read-iexprs-mixed-shallow
  (is= '(do (foo bar) 5 (9 10)) (read-iexprs (input "\nfoo bar\n5\n9 10"))))

(deftest read-iexprs-deeper
  (is=
    '(do (foo bar baz) 5 (6 7) (9 10))
    (read-iexprs (input "\nfoo bar\n\tbaz\n5\n6\n\t7\n9 10"))))
