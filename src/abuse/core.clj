; ## Clojure Reader Macros
; 
; This module abuses clojures internals to allow you to get and set reader
; macros. They are applied globally, are not thread-safe and depend on
; internal implementation details; it should probably be considered dangerous.
(ns abuse.core
    (:import [clojure.lang LispReader]))

; This gets the value of a field from a Java class, making it public in the
; process.
(defn get-publicize-field
  ([class name]
    (get-publicize-field class name nil))
  ([class name instance]
    (let [field (.getDeclaredField class name)]
         (.setAccessible field true)
         (.get field instance))))

; We'll begin by getting references to the arrays containg the reader macros
; and dispatch macros. They're indexed on a `char`.
(def reader-macros (get-publicize-field LispReader "macros"))
(def reader-dispatch-macros (get-publicize-field LispReader "dispatchMacros"))

; Reader macros are identified by a character or a string that must itself
; be a single character or a single character prefixed by `#` to define
; a dispatch macro.
(defn get-reader-macro
  [symbol]
    (cond
      (char? symbol)
        (locking reader-macros
          (aget reader-macros (int symbol)))
      (= (count symbol) 1)
        (locking reader-macros
          (aget reader-macros (int (first symbol))))
      (and (= (count symbol) 2) (= (first symbol) \#))
        (locking reader-dispatch-macros
          (aget reader-dispatch-macros (int (first (rest symbol)))))
      :else
        (throw (Exception. "Invalid symbol for reader macro."))))

; This returns the previous reader macro (usually hopefully `nil`).
(defn set-reader-macro
  [symbol fun]
    (cond
      (char? symbol)
        (locking reader-macros
          (let [old (aget reader-macros (int symbol) fun)]
               (aset reader-macros (int symbol) fun)
               old))
      (= (count symbol) 1)
        (locking reader-macros
          (let [old (aget reader-macros (int (first symbol)) fun)]
               (aset reader-macros (int (first symbol)) fun)
               old))
      (and (= (count symbol) 2) (= (first symbol) "#"))
        (locking reader-dispatch-macros
          (let [old (aset reader-dispatch-macros 
                          (int (first (rest symbol))) fun)]
               (aset reader-dispatch-macros (int (first (rest symbol))) fun)
               old))
      :else
        (throw (Exception. "Invalid symbol for reader macro."))))
