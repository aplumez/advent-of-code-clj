(ns advent-of-code-clj.2023.day01
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [clojure.string :as str]))

(def input (-> (puzzle 2023 1)
               (str/split #"\n")))

(def string->number
  {"one"   "1"
   "two"   "2"
   "three" "3"
   "four"  "4"
   "five"  "5"
   "six"   "6"
   "seven" "7"
   "eight" "8"
   "nine"  "9"})

(def solution1
  (->> input
       (map #(str/replace % #"[a-z]" ""))
       (map #(-> (str (first %) (last %))
                 (parse-long)))
       (reduce +)))

(defn- compute-digits
  [string m]
  (let [re-groups (-> (re-pattern (str "(?=(\\d|" (str/join "|" (keys m)) "))"))
                      (re-seq string))
        results (map second re-groups)]
    (->> results
         (#(list (first %) (last %)))
         (map #(or (get m %) %))
         (apply str)
         (parse-long))))

(def solution2
  (->> input
       (map #(compute-digits % string->number))
       (reduce +)))
