(ns reader-abuse.trying
    (:require reader-abuse.iexprs)
    (:require reader-abuse.infix))
#I

ns reader-abuse.trying
  :require
    [clojure.pprint :as pprint]

defn take-all
  "(take n s) or nil if #[n < (count s)]."
  [n s]
    let [v (vec (take n s))]
      if #[(count v) < n]
        v

defn chains
  "Returns a lazy sequence of all length n subsequences of s."
  [n s]
    if (first s)
      lazy-seq
        take-all n (first s)
        chains n (next s)

defn vec-split-last
  [v]
    let [n (count v)]
      list
        subvec v 0 #[n - 1]
        nth #[n - 1] v

defn markov-chains
  "Returns a nested map representing a markov chain of length n based on s."
  [n s]
    let [v (vec (seq s))]
      ;else
        into (hash-map)
          map
            comp vec-split-last
              fn [[key value]]
                list
                  key
                  if #[n > 1]
                    markov-chains #[n - 1] s
                    value
            chains #[n + 1] s

println
  let [f (markov-chains 1 ["hello"
                          "world"
                          "where"
                          "in"
                          "the"
                          "world"
                          "hello"
                          "viewers"])]
    println ((f))
