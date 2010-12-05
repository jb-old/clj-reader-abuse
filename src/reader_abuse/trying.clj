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
    if #[(count v) = n]
      v

defn chains [n s]
  "Returns a lazy sequence of all length n subsequences of s."
  
  let [link (take-all n s)]
    if link
      lazy-seq
        cons
          link
          chains n (rest s)

defn vec-split-last [v]
  let [n (count v)]
    list
      subvec v 0 #[n - 1]
      nth v #[n - 1]

defn following-occurrences
  "Given a sequence of 2-item sequences and returns a map from items that
  occur first to a map from the items that appear after them to the number of
  times they do so."
  
  [s]
    following-occurrences s {}
  
  [s m]
    let [[tail head] (first s)
          new-counts (merge-with #(merge-with + %1 %2)
                                 m { tail { head 1 } })]
        if (seq (rest s))
          recur (rest s) new-counts
          new-counts

defn map-keys [f m]
  "Maps a function over the keys of a map."
  
  into {}
    map
      fn [[key_ value]]
        list (f key_) value
      m

defn map-values [f m]
  "Maps a function over the values of a map."
  
  into {}
    map
      fn [[key_ value]]
        list key_ (f value)
      m

defn following-weights [s]
  map-values
    fn [[value occurrences]]
      {:value value :weight occurrences}
    following-occurrences s

println
  "<pre>"
  following-occurrences
    map
      vec-split-last
      chains 2 "aabcdadbc"
  "</pre>"
