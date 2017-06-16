(ns looping-is-recursion)

(defn power-helper [base exp res]
  (cond
    (= 0 exp) 1
    (= 1 exp) base
    (= 2 exp) (* res base)
    :else (power-helper base (dec exp) (* base base))))

(defn power [base exp]
  (power-helper base exp base))

(defn last-element [a-seq]
  (cond
    (empty? a-seq) nil
    (= 1 (count a-seq)) (first a-seq)
    :else (last-element (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (and (empty? seq1) (empty? seq2)) true
    (or (empty? seq1) (empty? seq2)) false
    (= (first seq1) (first seq2)) (seq= (rest seq1) (rest seq2))
    :else false))

(defn find-first-index [pred a-seq]
   (loop [index 0
          b-seq a-seq]
    (if (empty? b-seq)
      nil
      (if (pred (first b-seq))
        index
        (recur (inc index) (rest b-seq))))))

(defn avg [a-seq]
  (loop [b-seq a-seq
         sum   0]
    (if (empty? b-seq)
      (/ sum (count a-seq))
      (recur (rest b-seq) (+ sum (first b-seq))))))

(defn parity [a-seq]
  (loop [b-seq a-seq
         a-set #{}]
    (let [remove-elem (disj a-set (first b-seq))
          add-elem    (conj a-set (first b-seq))]
    (cond
      (empty? b-seq) a-set
      (contains? a-set (first b-seq)) (recur (rest b-seq) remove-elem)
      :else (recur (rest b-seq) add-elem)))))

(defn fast-fibo [n]
  (loop [n1 0
         n2 1
         i  0]
    (if (= i n)
      n1
      (recur (+ n1 n2) n1 (inc i)))))


(defn cut-at-repetition [a-seq]
  (loop [res   []
         b-seq a-seq]
    (if (or (empty? b-seq)
            (.contains res (first b-seq)))
      res
      (recur (conj res (first b-seq)) (rest b-seq)))))

