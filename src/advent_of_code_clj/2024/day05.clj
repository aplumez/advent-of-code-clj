(ns advent-of-code-clj.2024.day05
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [clojure.set :as set]
   [clojure.string :as string]))

(defonce input (->> (puzzle 2024 5)
                    (string/split-lines)))

(defn parse-rules
  [text]
  (->> text
       (filter #(string/includes? % "|"))
       (map #(string/split % #"\|"))
       (mapv (partial mapv parse-long))))

(defn rules-map
  [rules]
  (->> rules
       (reduce (fn [acc [a b]]
                 (merge-with set/union acc {a (set [b])}))
               {})))

(defn parse-updates
  [text]
  (->> text
       (filter #(string/includes? % ","))
       (map #(string/split % #","))
       (mapv (partial mapv parse-long))))

(defn incorrect-updates-line?
  [rules-map updates-line]
  (->> updates-line
       (reduce
        (fn [acc el]
          (if-not (->> (set acc)
                       (set/intersection (get rules-map el))
                       (empty?))
            (reduced nil)
            (conj acc el)))
        [])
       (nil?)))

(defn middle-item
  [coll]
  (-> coll count (/ 2) (#(nth coll %))))

(def solution1
  (let [rules-map (-> input parse-rules rules-map)]
    (->> (parse-updates input)
         (remove (partial incorrect-updates-line? rules-map))
         (map middle-item)
         (apply +))))

(defn sort-updates-line
  [rules-map updates-line]
  (sort (fn [a b]
          (if (-> (get rules-map a)
                  (contains? b))
            -1 0))))

(def solution2
  (let [rules-map (-> input parse-rules rules-map)]
    (->> (parse-updates input)
         (filter (partial incorrect-updates-line? rules-map))
         (map (partial sort (fn [a b]
                              (if (contains? (get rules-map a)
                                             b)
                                -1 0))))
         (map middle-item)
         (apply +))))
