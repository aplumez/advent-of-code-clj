(ns advent-of-code-clj.2023.day03
  (:require [advent-of-code-clj.core :refer [puzzle]]
            [advent-of-code-clj.utils :as u]
            [clojure.string :as str]))

(def input (-> (puzzle 2023 3)
               (str/split #"\n")))

(defn adjacent-cases
  [number r-index c-index]
  (for [x (range (dec c-index) (+ c-index (count number) 1))
        y (range (dec r-index) (+ r-index 2))]
    [y x]))

(def solution1
  (let [row-fn
        (fn [row-index row]
          (->> (u/re-seq-pos #"\d+" row)
               (map (fn [{:keys [start _end group]}]
                      (let [cases (adjacent-cases group row-index start)
                            adjacent? (some #(->> (get-in input % ".")
                                                  (str)
                                                  (re-find #"\d|\.")
                                                  (not))
                                            cases)]
                        (when adjacent? group))))))]
    (->> input
         (map-indexed row-fn)
         (flatten)
         (remove nil?)
         (map parse-long)
         (reduce +))))

(defn find-adjacent-digits
  [input y x]
  (for [t-x (range (dec x) (+ x 2))
        t-y (range (dec y) (+ y 2))
        :let [val (str (get-in input [t-y t-x] "."))]]
    (when (re-find #"\d" val) {:x t-x :y t-y})))

(defn find-complete-number
  [input y x]
  (let [[n1 n2 n3 n4 n5] (for [t-x (range (- x 2) (+ x 3))
                               :let [val (str (get-in input [y t-x] "."))]]
                           (when (re-find #"\d" val) val))
        [n1 n2 n3 n4 n5] (if-not n2
                           [nil nil n3 n4 n5]
                           [n1 n2 n3 n4 n5])
        [n1 n2 n3 n4 n5] (if-not n4
                           [n1 n2 n3 nil nil]
                           [n1 n2 n3 n4 n5])]
    (parse-long (str n1 n2 n3 n4 n5))))

(def solution2
  (->> input
       (map-indexed (fn [r-index row]
                      (->> (u/re-seq-pos #"\*" row)
                           (map #(hash-map :r-index r-index :match %)))))
       (flatten)
       (map (fn [{:keys [r-index match]}]
              (when-let [start (get match :start)]
                (find-adjacent-digits input r-index start))))
       (map (fn [matches]
              (->> matches
                   (remove nil?)
                   (map (fn [{:keys [x y]}]
                          (find-complete-number input y x)))
                   (set))))
       (remove #(< (count %) 2))
       (map #(reduce * %))
       (reduce +)))


;;[546563 91031374]
