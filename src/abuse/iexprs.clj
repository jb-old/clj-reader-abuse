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

(defn hws (set "\t "))

; Reads horizontal whitespace and returns a number indicating the number of
; characters read. Zero is okay.
(defn read-hws
  ([reader]
    (read-hws reader (.read reader) 0))
  
  ([reader initch]
    (read-hws reader initch 0))
  
  ([reader initch sum]
    (if (hws initch)
        (recur reader (.read reader) (+ 1 sum))
        (do (.unread reader initch)
            0))))

; Returns a number indicating the hws read on the first line with anything
; other than hws. Doesn't want an initch.
(defn read-hws-skippy
  [reader]
    (loop []
          (let [indent (read-hws reader) next-char (.read reader)]
               (if (not= next-char \newline)
                   (do (.unread reader next-char)
                       indent)
                   (recur)))))

; This is the function we're defining as the macro.
(defn read-iexprs
  [reader initch]
    (def first-char (.read reader))
    
    (def specified-indent
         (if (Character/isDigit first-char)
             ((get-read-method "Number") reader first-char)
             (.unread reader first-char)))
    
    (if (not= (char (.read reader)) \newline)
        (throw (Exception. "Newline must follow #I-exprs opening.")))
    
    ; We have to skip over blank lines.
    (def initial-indent (read-hws-skippy reader))
    (def indent-size (or specified-indent initial-indent))
    
    (#{-1 (int \newline)} (reader-peek reader))
    
)

(set-reader-macro "#I" read-iexprs)

#I
  foo to the bar yo
    yo

#I2
      eh eh eh
        ehhhhh
