(ns reader-abuse.iexprs
  (:use reader-abuse.iexprs)
  (:use clojure.test)
  (:import [clojure.lang LineNumberingPushbackReader])
  (:import [java.io StringReader]))

(defn input
  [s] (LineNumberingPushbackReader. (StringReader. s)))

; I'm not comfortable with the susinct notations used for macros.
(defmacro is=
  [& forms] (list 'is (list 'apply '= (vec forms))))

; read-hws, those tests are easy to write.
; oh good, I have figured out how to write a test
(deftest read-hws-null
  (is= 0 (read-hws (input ""))))

(deftest read-hws-some
  (let [in (input "  ")]
    (is= 2 (read-hws in))
    (is (#{-1 65535} (.read in)))))

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
(deftest read-iexpers-simple-delimiter-terminated
  (is= '(do 4) (read-iexprs (input "\n4)"))))

(deftest read-iexprs-simple-eof-terminated
  (is= '(do 3) (read-iexprs (input "\n3"))))
