(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [result exp]
                 (if (zero? exp)
                   result
                   (recur (* result base) (dec exp))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (cond
                   (empty? a-seq)
                     nil
                   (= (count a-seq) 1)
                     (first a-seq)
                   :else
                     (recur (rest a-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                   (cond
                     (and (empty? s1) (empty? s2)) true
                     (or (empty? s1) (empty? s2)) false
                     (not (= (first s1) (first s2))) false
                     :else (recur (rest s1) (rest s2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [i 0 mapped-seq (map pred a-seq)]
    (cond
      (empty? mapped-seq) nil
      (first mapped-seq) i
      :else (recur (inc i) (rest mapped-seq)))))

(defn avg [a-seq]
  (loop [n 0
         r-seq a-seq
         sum 0]
    (if (empty? r-seq)
      (/ sum n)
      (recur (inc n) (rest r-seq) (+ sum (first r-seq))))))


(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [retval #{}
         rest-seq a-seq]
    (if (empty? rest-seq)
      retval
      (recur (toggle retval (first rest-seq)) (rest rest-seq)))))

(defn fast-fibo [n]
  (cond
    (= n 0) 0
    (= n 1) 1
    :else
      (loop [i n
             x1 0
             x2 1]
        (if (= i 1)
          x2
          (recur (dec i) x2 (+ x1 x2))))))

(defn cut-at-repetition [a-seq]
  (loop [retval []
         inner-seq a-seq]
    (cond
      (empty? inner-seq) retval
      (some (conj #{} (first inner-seq)) retval) retval
      :else (recur (conj retval (first inner-seq)) (rest inner-seq)))))
