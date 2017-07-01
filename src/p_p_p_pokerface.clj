(ns p-p-p-pokerface)

(defn rank [card]
  (let [[f s] card
        tens {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    (if (Character/isDigit f)
      (Integer/valueOf (str f))
      (get tens f))))

(defn suit [card]
  (let [[r s] card]
    (str s)))

(defn ranks [hand]
  (map rank hand))

(defn suits [hand]
  (map suit hand))

(defn rank-freqs [hand]
  (frequencies (ranks hand)))

(defn suit-freqs [hand]
  (frequencies (suits hand)))

(defn max-same [hand]
  (apply max (vals (rank-freqs hand))))

(defn max-same-suit [hand]
  (apply max (vals (suit-freqs hand))))

(defn pair? [hand]
  (<= 2 (max-same hand)))

(defn three-of-a-kind? [hand]
  (<= 3 (max-same hand)))

(defn four-of-a-kind? [hand]
  (<= 4 (max-same hand)))

(defn flush? [hand]
  (= 5 (max-same-suit hand)))

(defn full-house? [hand]
  (let [freqs (set (vals (rank-freqs hand)))]
    (and
      (contains? freqs 2)
      (contains? freqs 3))))


(defn two-pairs? [hand]
  (let [pairs (get (frequencies (vals (rank-freqs hand))) 2)]
    (or
      (full-house? hand)
      (= 2 pairs))))

(defn straight-ranks? [ranks]
  (let [sorted (sort ranks)
        straight (range (first sorted) (+ 5 (first sorted)))]
    (= sorted straight)))



(defn straight? [hand]
  (let [ranks (ranks hand)
        replaced-ranks (replace {14 1} ranks)]
    (or
      (straight-ranks? ranks)
      (straight-ranks? replaced-ranks))))


(defn straight-flush? [hand]
  (and
    (straight? hand)
    (flush? hand)))

(defn high-card? [hand] true)

(def checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]})



(defn value [hand]
  (apply
    max
    (map
      second
      (filter
        (fn [[checkerfn]]
          (checkerfn hand))
        checkers))))
