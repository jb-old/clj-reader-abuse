(ns abuse.all
    (:require abuse.core
              abuse.iexprs
              abuse.infix))
#I

defn ** [a b]
  Math/pow a b

defmacro domacro [& body]
  `(#I
      defmacro domacro# []
        ~@body
      (domacro#))

defmacro let* [& pairs]
  let [symbol (first (last pairs))]
    list `let (vec (for [[name value] pairs] [name value])) symbol
