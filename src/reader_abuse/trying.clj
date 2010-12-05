(ns reader-abuse.trying
    (:require reader-abuse.iexprs)
    (:require reader-abuse.infix))

#I

ns reader-abuse.trying
  :require
    [clojure.pprint :as pprint]

defn normalize-weights [weighted-values]
  "Normalizes the weights in a sequence of maps with :weight and :value."
  
  let [weight-sum (reduce + (map :weight weighted-values))]
    for [item weighted-values]
      {:weight #[(:weight item) / weight-sum]
       :value (:value item)}

defn weighted-sample [normalized-weighted-values]
  "Samples an object from a sequence of maps with :weight and :value, where
  the weights are normalized to have a sum of 1."
  
  loop [remaining-values normalized-weighted-values
        remaining-space (rand)]
    let [current-weight (:weight (first remaining-values))]
      if #[remaining-space < current-weight]
        (first remaining-values) :value
        recur (rest remaining-values) #[remaining-space - current-weight]

defn take-all [n s]
  "(take n s) or nil if #[n < (count s)]."
  
  let [v (vec (take n s))]
    if #[(count v) < n]
      v

defn chains [n s]
  "Returns a lazy sequence of all length n subsequences of s."
  
    if (first s)
      lazy-seq
        take-all n (first s)
        chains n (next s)

defn vec-split-last [v]
  let [n (count v)]
    list
      subvec v 0 #[n - 1]
      nth #[n - 1] v

defn following-frequencies
  [s]
    following-frequencies s {}
  
  [s m]
    let [[key value] (first s)
         new-frequencies (merge-with + {  } )
  


defn get-one []
  weighted-sample
    normalize-weights
      list
        { :weight 5 :value "A" }
        { :weight 2 :value "B" }

println (get-one)
println (get-one)
println (get-one)
println (get-one)
println (get-one)
println (get-one)
println (get-one)
println (get-one)
println (get-one)
println (get-one)
