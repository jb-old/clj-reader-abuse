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

; The ints corresponding to EOF and the closing brackets. Used to denote the
; end of an #I-expressions block.
(def terminator-ints #{-1 nil (int \)) (int \}) (int \])})

; Adds newline, these all will indicate the end of a line in #EEEEE
(def terminator-and-newline-ints (conj terminator-ints (int \newline)))

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
        (do
          (.unread reader initial-char)
          sum))))

; Behaves as `read-hws` except that it skips over lines containing nothing
; but horizontal whitespace.
(defn read-hws-skippy
  [reader]
    (let [indent (read-hws reader) next-char (.read reader)]
         (if (not= (char next-char) \newline)
             (do (.unread reader next-char)
                 indent)
             (recur reader))))

(defn line
  [indentation forms]
    { :indentation indentation :forms forms })

; Returns a vec of forms encountered before a \newline.
(defn read-to-eol
  [reader]
    (loop
      [forms []]
      (if (terminator-and-newline-ints (reader-peek reader))
        forms
        (let
          [new-forms (conj forms (read))]
          (read-hws reader)
          (recur new-forms)))))

; This is the reader function we'll be defining as our `#I` macro.
(defn read-iexprs
  [reader initial-char]
    (if (not= (char (.read reader)) \newline)
        (throw (Exception. "Newline must follow #I-exprs opening.")))
    
    (def initial-indent (read-hws-skippy reader))
    (def first-line (read-to-eol reader))
    
    (loop
      [lines [{:indentation initial-indent :forms first-line}]]
      (if
        (terminator-ints (reader-peek reader))
        lines
        (do
          (.read reader) ; it will be a newline
          (recur
            (let
              [indentation (read-hws reader)
               forms (read-to-eol reader)]
               (if
                 (> (count forms) 0)
                 (conj lines {:indentation indentation :forms forms})
                 lines)))))))

(set-reader-macro "#I" read-iexprs)
(set-reader-macro "#D" (fn [r c] (println "!" (reader-peek r))))

(println #I
  2 3 :foo 4
    9)

(println #I
      :a :b :c
        :d
        :e
          :g
        :h
          :i
      :j
)