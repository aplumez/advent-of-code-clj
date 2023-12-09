(ns advent-of-code-clj.2023.day07
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [clojure.string :as str]))

(defn replacements [with-joker?]
  [["A" "E"] ["K" "D"] ["Q" "C"] ["J" (if with-joker? "0" "B")] ["T" "A"]])

(defn input
  [with-joker?]
  (-> (reduce #(str/replace %1 (first %2) (last %2))
              (puzzle 2023 7)
              (replacements with-joker?))
      (str/split-lines)))

(defn hand-strength [hand]
  (let [freq (->> (frequencies hand)
                  (filter #(and (> (val %) 1) (not= (key %) \0)))
                  (into {}))
        nb-jokers (count (filter #(= "0" %) (str/split hand #"")))
        max-freq (if (empty? (vals freq))
                   (if (zero? nb-jokers) 1 (if (= 5 nb-jokers) 5 (inc nb-jokers)))
                   (+ (apply max (vals freq)) nb-jokers))]
    (case max-freq
      1 1
      2 (if (= (count freq) 2) 3 2)
      3 (if (= (count freq) 2) 5 4)
      4 6
      5 7)))

(defn parse-line [line]
  (let [[hand bid] (str/split line #" ")]
    [(parse-long bid) (str (hand-strength hand) hand)]))

(defn solve [with-joker?]
  (->> (input with-joker?)
       (mapv parse-line)
       (sort-by last)
       (map-indexed #(* (inc %1) (first %2)))
       (reduce +)))

(def solution1 (solve false))

(def solution2 (solve true))
