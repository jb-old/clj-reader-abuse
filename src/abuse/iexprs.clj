; ## #I-expressions
; 
; This module provides add `#I` reader macro inspired by the I-expressions
; described in [SRFI-49](http://goo.gl/32CgC). `#I` casues the text following
; it to be parsed based on indentation. If the first line is indented then the
; rule will continue until encountering a line that is less indented than the
; first. If the first line is not indented then the only way to terminate it
; is a closing bracket matching an opening one that occured before `#I`. The
; indented expressions will be wrapped in a `do`. Lines that are empty except
; for horizontal whitespace characters are ignroed. Ordinary Clojure 
; expressions are parsed, with indentation rules not applied inside of them.
; 
; SRFI-49 introduces a `group` keyword used to allow you to have a list as the
; first item in a list, something that this syntax does not otherwise
; accomidate. I am adverse to adding a keyword to the language so at present
; if you require this it will have to be written as a regular Clojure
; expression.

(ns abuse.iexprs
    (:use abuse.core))

; Horizontal whitespace characters used to denote indentation. Note that as in
; Python tabs and spaces are treated equivilently.
(def hws (set "\t "))

; Reads horizontal whitespace and returns a number indicating the number of
; characters read. Zero is okay, this shouldn't raise an exception.
(defn read-hws
  ([reader]
    (read-hws reader (.read reader) 0))
  
  ([reader initial-char]
    (read-hws reader initial-char 0))
  
  ([reader initial-char sum]
    (if (hws initial-char)
        (recur reader (.read reader) (+ 1 sum))
        (do (.unread reader initial-char)
            0))))

; Behaves as `read-hws` except that it skips over lines containing nothing
; but horizontal whitespace.
(defn read-hws-skippy
  [reader]
    (let [indent (read-hws reader) next-char (.read reader)]
         (if (not= (char next-char) \newline)
             (do (.unread reader next-char)
                 indent)
             (recur reader))))

; This is the reader function we'll be defining as our `#I` macro.
(defn read-iexprs
  [reader initial-char]
    (if (not= (char (.read reader)) \newline)
        (throw (Exception. "Newline must follow #I-exprs opening.")))
    
    (def initial-indent (read-hws-skippy reader))
    
    ( { :indent })
    
    (def first-form (read-form reader))
    (read-hws reader)
    (if (not= (char (reader-peek reader)) \newline))
    
    ; Should make this a function that's called recursively for each level
    ; of indentation.
    ; Have a list stack (itself a list) containg vecs. Each token is conjed
    ; onto the current top. When indentation increases we throw a new one on,
    ; when indentation decreases we top the latest vec off, convert it to a
    ; list and put it on the one below.
    ; 
    ; This would be easy with more state, but it feels like it should also be
    ; really easy to do functionally. I need to develop my thinking.
    
    (#{-1 (int \newline)} (reader-peek reader))

)

(set-reader-macro "#I" read-iexprs)

#I
  foo to the bar yo
    yo

#I2
      eh eh eh
        ehhhhh
