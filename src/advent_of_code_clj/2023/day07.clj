(ns advent-of-code-clj.2023.day07
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [clojure.string :as str]))

(def example "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483")

(defn input
  [with-joker?]
  (-> (puzzle 2023 7)
      (str/replace "A" "E")
      (str/replace "K" "D")
      (str/replace "Q" "C")
      (str/replace "J" (if with-joker? "0" "B"))
      (str/replace "T" "A")
      (str/split-lines)))

(def solution1
  (->> (input false)
       (mapv (fn [x]
               (let [[hand bid] (str/split x #" ")
                     freq (->> (frequencies hand)
                               (filter #(> (val %) 1))
                               (into {}))
                     max-freq (if (empty? (vals freq)) 1 (apply max (vals freq)))
                     hand-strength (case max-freq
                                     1 1
                                     2 (if (= (count freq) 1) 2 3)
                                     3 (if (= (count freq) 1) 4 5)
                                     4 6
                                     5 7)]
                 [hand (parse-long bid) (str hand-strength "-" hand)])))
       (sort-by last)
       (map-indexed (fn [index [_ bid _]]
                      (* (inc index) bid)))
       (reduce +)))

(def solution2
  (->> (input true)
       (mapv (fn [x]
               (let [[hand bid] (str/split x #" ")
                     freq (->> (frequencies hand)
                               (filter #(and (> (val %) 1) (not= (key %) \0)))
                               (into {}))
                     nb-jokers (->> (str/split hand #"")
                                    (filter #(= "0" %))
                                    (count))
                     max-freq (if (empty? (vals freq))
                                (if (zero? nb-jokers) 1 (if (= 5 nb-jokers) 5 (inc nb-jokers)))
                                (+ (apply max (vals freq)) nb-jokers))
                     hand-strength (case max-freq
                                     1 1
                                     2 (if (= (count freq) 2) 3 2)
                                     3 (if (= (count freq) 2) 5 4)
                                     4 6
                                     5 7)]
                 [hand (parse-long bid) hand-strength (str hand-strength "-" hand)])))
       (sort-by last)
       (map-indexed (fn [index [_ bid _]]
                      (* (inc index) bid)))
       (reduce +)))
