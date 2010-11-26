; ## #I-expressions
; 
; This module provides add `#I` reader macro inspired by the I-expressions
; described in [SRFI-49](http://goo.gl/32CgC).
; 
; describing is hard.
; 
; - If it's not indented it continues to EOF.
; - If a number is specified after `#I` that is the indent width.  
;   Else the indent width is the leading indentation of the first line.
; - If no indentation width is specified and the first line is not indented
;   then you are unable to indent. Don't do that.
; - Each multiple of the indent width corresponds to a wrapping pair of 
;   parens.
; - Indentation is ignored inside of `()`/`{}`/`[]`.
; - A single item with nothing else on its line or indented following it will
;   not be wrapped in parens.
; - `#I` is terminated before an unmatched `)`/`}`/`]`, an unindented line
;   if its contents are indented, or EOF.
;
; Here's an example using `let` incorrectly.
; 
;     #I
;       let
;           x 2
;           y 3
;         + x y
;     
;       defn foo
;         [bar]
;           + bar (foo)
;         []
;           3
; 
;     (let
;       ((x 2)
;        (y 2))
;       (+ x y))
;     
;     (defn foo
;       ([bar]
;         (+ 2 (foo)))
;       ([]
;         3))
; 
; Here's an example showing specified indentation.
; 
;     (defn foo
;        []
;          (do #I2
;             if (> 3 1)
;               println "foo"
;               println "bar"))
;
;     (defn foo
;        []
;          (do
;             (if (> 3 1)
;               (println "foo")
;               (println "bar"))))
; 
; I'll begin with naive implementation which looks exclusively at indentation.

(ns abuse.iexprs
    (:use abuse.core)
    (:import [clojure.lang LispReader]))

(defn read-iexprs
  [reader initch]
    (def first-following (.read reader))
    (.unread reader first-following)
    (def specified-indentation
         (if (Character/isDigit first-following)
             ((get-publicize-field LispReader "readNumber") reader)))
    (if (not= (char (.read reader)) \newline)
        (throw (Exception. "INVALID WILL DESCRIBE LATER")))
    
  )

(set-reader-macro "#I" read-iexprs)
