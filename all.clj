(ns abuse.all
    (:require abuse.core
              abuse.iexprs
              abuse.infix))
#I

def ** Math/pow

defmacro domacro [& body]
  syntax-quote do
      defmacro domacro# []
        ~@body
      (domacro#)

defmacro let* [& pairs]
  let [symbol (first (last pairs))]
    list `let vec pairs for [[name value] pairs] [name value] symbol
