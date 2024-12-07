(ns advent-of-code-clj.2024.day07
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [clojure.string :as string]
   [clojure.math :as math]))

(def input
  (->> (puzzle 2024 7)
       (string/split-lines)
       (map #(string/split % #":"))
       (map (fn [[result numbers]]
              [(parse-long result)
               (mapv parse-long (-> numbers
                                    string/trim
                                    (string/split #" ")))]))))

(defn padded-base
  [n base length]
  (-> (Integer/toString n base)
      ((partial format (str "%" length "s")))
      (string/replace " " "0")))

(defn solve-line
  [base [result numbers]]
  (let [combinations (int (math/pow base (dec (count numbers))))]
    (loop [combination 0]
      (if (= combination combinations)
        0
        (let [binary-combination
              (-> combination
                  (padded-base base (dec (count numbers)))
                  (string/split #""))
              [_ combination-result]
              (reduce
               (fn [[c res] el]
                 (case (first c)
                   "0" [(rest c) (* res el)]
                   "1" [(rest c) (+ res el)]
                   "2" [(rest c) (parse-long (str res el))]))
               [binary-combination (first numbers)]
               (rest numbers))]
          (if (= result combination-result)
            result
            (recur (inc combination))))))))

(defn solve
  [base input]
  (->> input
       (map (partial solve-line base))
       (reduce +)))

(def solution1
  (solve 2 input))

(def solution2
  (solve 3 input))
