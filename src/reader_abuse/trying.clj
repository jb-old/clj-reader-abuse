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
        [(f key_) value]
      m

defn map-values [f m]
  "Maps a function over the values of a map."
  
  into {}
    map
      fn [[key_ value]]
        [key_ (f value)]
      m

defn following-weights [s]
  map-values
    fn [value]
      map
        fn [[key_ occurrences]]
          { :value key_ :weight occurrences }
        value
    following-occurrences s

defn following-weights-normalized [s]
  map-values
    normalize-weights
    following-weights s

defn markov-evaluate [n s]
  following-weights-normalized
    map
      vec-split-last
      chains #[n + 1] (concat [::start] s [::end])

defn markov-sample
  [weights starting-point]
    weighted-sample (weights (vec starting-point))
  
  [weights]
    markov-sample weights (rand-nth (keys weights))

defn markov-sample-chain
  [weights starting-point]
    let [sample (markov-sample weights starting-point)]
      if #[sample not= ::end]
        lazy-seq
          cons
            sample
            markov-sample-chain weights
              conj
                subvec (vec starting-point) 1
                sample
  
  [weights]
    markov-sample-chain weights
      rand-nth (vec (filter #(= ::start (first %))) (weights :keys))

def lipsum-sample
  markov-evaluate 3 "But I must explain to you how all this mistaken idea of denouncing pleasure and praising pain was born and I will give you a complete account of the system, and expound the actual teachings of the great explorer of the truth, the master-builder of human happiness. No one rejects, dislikes, or avoids pleasure itself, because it is pleasure, but because those who do not know how to pursue pleasure rationally encounter consequences that are extremely painful. Nor again is there anyone who loves or pursues or desires to obtain pain of itself, because it is pain, but occasionally circumstances occur in which toil and pain can procure him some great pleasure. To take a trivial example, which of us ever undertakes laborious physical exercise, except to obtain some advantage from it? But who has any right to find fault with a man who chooses to enjoy a pleasure that has no annoying consequences, or one who avoids a pain that produces no resultant pleasure? On the other hand, we denounce with righteous indignation and dislike men who are so beguiled and demoralized by the charms of pleasure of the moment, so blinded by desire, that they cannot foresee the pain and trouble that are bound to ensue; and equal blame belongs to those who fail in their duty through weakness of will, which is the same as saying through shrinking from toil and pain. These cases are perfectly simple and easy to distinguish. In a free hour, when our power of choice is untrammelled and when nothing prevents our being able to do what we like best, every pleasure is to be welcomed and every pain avoided. But in certain circumstances and owing to the claims of duty or the obligations of business it will frequently occur that pleasures have to be repudiated and annoyances accepted. The wise man therefore always holds in these matters to this principle of selection: he rejects pleasures to secure other greater pleasures, or else he endures pains to avoid worse pains."

println
  "<pre>"
  apply str "N" (markov-sample-chain lipsum-sample ". N")
  "</pre>"
