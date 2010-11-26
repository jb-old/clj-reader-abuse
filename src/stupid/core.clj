; ## Oh No Clojure!
; ### Ramblingtroduction
; I saw [Sweet-expressions](http://goo.gl/oGzNs). It has good idea and made
; some of my existing ones click with their lispful potential. Saw that he
; introduced new syntax from within the language itself. Starting researching,
; now I know what reader macros are. Also that Clojure doesn't support them.
; I agree with the reasoning, but would like to do it anyway, and found [a
; blog post](http://goo.gl/aGpNU) which explains that since the language core
; is in Java you can just mess with it directly to do what you want, and
; provided an example. Marvelous.
(ns stupid.core)

; We'll begin by getting references to the arrays containg the reader macros
; and dispatch macros. They're indexed on a `char`.
(def reader-macros
  ; This gets the [`Field`](http://goo.gl/yJV8Z) containing the reader
  ; macros. It's private
  (let [field
         (.getDeclaredField clojure.lang.LispReader "macros")]
       ; so we make it public
       (.setAccessible true field)
       ; and get the value. If we were getting a non-static field we would
       ; specify an instance instead of `nil`.
       (.get field nil)))

(def reader-dispatch-macros
  (let [field
         (.getDeclaredField clojure.lang.LispReader "dispatchMacros")]
       (.setAccessible true field)
       (.get field nil)))

; This function is used to define a reader macro. It takes a character or
; a string identifying which it is setting and a function to set it to.
(defn def-reader-macro
  [symbol fun]
    (cond
      (char? symbol)
        (aset reader-macros (int symbol) fun)
      (= (count symbol) 1)
        (aset reader-macros (int (first symbol)) fun)
      (and (= (count symbol) 2) (= (first symbol) "#"))
        (aset dispatch-reader-macros (int (first (rest symbol))) fun)
      :else
        (throw (Exception. "Bad symbol for reader macro."))))

