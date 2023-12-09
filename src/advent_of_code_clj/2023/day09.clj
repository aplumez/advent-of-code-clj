(ns advent-of-code-clj.2023.day09
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [clojure.string :as str]))

(def input (->> (puzzle 2023 9)
                (str/split-lines)
                (mapv #(->> (str/split % #" ")
                            (mapv parse-long)))))

(def example-input [[0 3 6 9 12 15]
                    [1 3 6 10 15 21]
                    [10 13 16 21 30 45]])

(defn compute-jumps [seq]
  (->> seq
       (map-indexed (fn [index current-value]
                      (when-let [last-value (get seq (dec index))]
                        (- current-value last-value))))
       (remove nil?)
       (vec)))

(defn compute-jumps-history [seq]
  (loop [history []
         seq seq]
    (if (every? zero? seq)
      (conj history seq)
      (recur (conj history seq) (compute-jumps seq)))))

(defn find-prediction [jumps-history]
  (let [jumps-history (-> jumps-history reverse vec)]
    (loop [index 0
           last-prediction 0]
      (let [current-line (get jumps-history index)
            line-prediction (+ (last current-line) last-prediction)]
        (if (< (inc index) (count jumps-history))
          (recur (inc index) line-prediction)
          line-prediction)))))

(def solution1
  (->> (for [history input]
         (let [jumps-history (compute-jumps-history history)]
           (find-prediction jumps-history)))
       (reduce +)))

(defn find-first-prediction [jumps-history]
  (let [jumps-history (-> jumps-history reverse vec)]
    (loop [index 0
           last-prediction 0]
      (let [current-line (get jumps-history index)
            line-prediction (- (first current-line) last-prediction)]
        (if (< (inc index) (count jumps-history))
          (recur (inc index) line-prediction)
          line-prediction)))))

(def solution2
  (->> (for [history input]
         (let [jumps-history (compute-jumps-history history)]
           (find-first-prediction jumps-history)))
       (reduce +)))
