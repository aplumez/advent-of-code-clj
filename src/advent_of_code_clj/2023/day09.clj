(ns advent-of-code-clj.2023.day09
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [clojure.string :as str]))

(def input (->> (str/split-lines (puzzle 2023 9))
                (mapv #(mapv parse-long (str/split % #" ")))))

(defn compute-jumps [seq]
  (->> (map-indexed #(when-let [last-value (get seq (dec %1))] (- %2 last-value)) seq)
       (remove nil?)
       (vec)))

(defn compute-jumps-history [seq]
  (loop [history [], seq seq]
    (if (every? zero? seq)
      (conj history seq)
      (recur (conj history seq) (compute-jumps seq)))))

(defn find-prediction [jumps-history type]
  (let [jumps-history (-> jumps-history reverse vec)]
    (loop [index 0, last-prediction 0]
      (let [current-line (get jumps-history index)
            line-prediction (case type
                              :start (- (first current-line) last-prediction)
                              :end (+ (last current-line) last-prediction))]
        (if (< (inc index) (count jumps-history))
          (recur (inc index) line-prediction)
          line-prediction)))))

(defn solve [type]
  (->> (mapv #(-> (compute-jumps-history %) (find-prediction type)) input)
       (reduce +)))

(def solution1 (solve :end))

(def solution2 (solve :start))
