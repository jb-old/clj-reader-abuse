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
    (:use abuse.core)
    (:use abuse.hook) (:use clojure.pprint)) ; for nix this when finished

; Horizontal whitespace characters used to denote indentation. Note that as in
; Python tabs and spaces are treated equivilently.
(def hws-ints #{(int \t) 32})

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
    (if (hws-ints initial-char)
        (recur reader (.read reader) (+ 1 sum))
        (do
          (.unread reader initial-char)
          sum))))

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
          [new-forms (conj forms (read reader))]
          (read-hws reader)
          (recur new-forms)))))

(defn read-iexpr-lines
  ([reader]
    (read-iexpr-lines reader []))
  
  ([reader lines]
    (if
      (terminator-ints (reader-peek reader))
      lines
      (do
        (.read reader) ; it will be a newline
        (recur
          reader
          (let
            [line-number (.getLineNumber reader)
             indentation (read-hws reader)
             forms (read-to-eol reader)]
             (if
               (> (count forms) 0) ; don't care 'bout those empty lines!
               (conj lines
                 {:indentation indentation
                  :forms forms
                  :number line-number})
               lines)))))))

; `[a b c]` -> `[[a b c] [b c] [c]]`
(defn super-seq
  [s]
    (if s
      (lazy-seq (cons
        s
        (super-seq (rest s))))))

; bug: ignores c in
; 
;     a
;       b
;      c
(defn interpret-next-indented-line
  [lines]
    (if (seq lines)
      (let
        [{initial-indent :indentation first-forms :forms} (first lines)
         subsequent-line (first (rest lines))
         rest-forms (for
           [line* (super-seq (rest lines))
            :while (>=
              (:indentation (first line*))
              (subsequent-line :indentation))
            :when (=
              (:indentation (first line*))
              (subsequent-line :indentation))]
           (interpret-next-indented-line line*))
         foo (pprint rest-forms)
         forms (concat first-forms rest-forms)]
        (if (> (count forms) 1)
          (list* forms)
          (first forms)))))

; This is the reader function we'll be defining as our `#I` macro.
(defn read-iexprs
  [reader initial-char]
    (read-hws reader)
    (let [initial-line-number (.getLineNumber reader)
          first-form (if (not= (char (reader-peek reader)) \newline)
                           (read reader)
                           'do)
          lines (read-iexpr-lines reader [{
            :indentation -1
            :forms [first-form]
            :number initial-line-number}])]
          (interpret-next-indented-line lines)))

(set-reader-macro "#I" read-iexprs)

(println "<pre>")

(pprint '#I
  2 3 :foo 4
    9)

(pprint '#I pprint
      (:a :b :c)
        [:d]
        #{:e}
          :g
        :h
          :i
      :j
)

(println "</pre>")
