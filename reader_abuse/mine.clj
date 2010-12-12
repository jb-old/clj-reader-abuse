(ns reader-abuse.hook
    (:require reader-abuse.core
              reader-abuse.iexprs
              reader-abuse.infix))

(do #I

def ** Math/pow

defmacro domacro [& body]
  syntax-quote do
      defmacro domacro# []
        ~@body
      (domacro#)

defmacro let* [& pairs]
  let [value (first (last pairs))]
    list `let vec pairs for [[name value] pairs] [name value] symbol


) ; because of iexprs failure
