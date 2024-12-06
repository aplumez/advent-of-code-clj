(ns advent-of-code-clj.2024.day06
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [clojure.string :as string]))

(defonce input
  (->> (puzzle 2024 6)
       (string/split-lines)
       (mapv #(string/split % #""))))

(defn guard-position
  [plan]
  (let [y (->> plan
               (map set)
               (take-while (complement #(contains? % "^")))
               (count))
        x (->> (nth plan y)
               (take-while (complement #{"^"}))
               (count))]
    [x y]))

(defn out-of-plan?
  [plan [x y]]
  (not (and (<= 0 x (dec (count (first plan))))
            (<= 0 y (dec (count plan))))))

(defn next-position
  [[x y] [dir-x dir-y]]
  [(+ x dir-x) (+ y dir-y)])

(defn guard-path
  [plan]
  (loop [guard-position (guard-position plan)
         [dir-x dir-y] [0 -1]
         visited #{}]
    (let [[next-x next-y] (next-position guard-position [dir-x dir-y])]
      (if (out-of-plan? plan [next-x next-y])
        (conj visited guard-position)
        (if (= "#" (-> plan (nth next-y) (nth next-x)))
          (recur guard-position [(- dir-y) dir-x] visited)
          (recur [next-x next-y] [dir-x dir-y] (conj visited guard-position)))))))

(def solution1
  (->> (guard-path input)
       (count)))

(defn plans-with-obstacle
  [initial-plan]
  (->> (guard-path initial-plan)
       (map (fn [[x y]]
              (when (not= "^" (get-in initial-plan [y x]))
                (assoc-in initial-plan [y x] "#"))))
       (remove nil?)))

(def solution2
  (->> (plans-with-obstacle input)
       (reduce
        (fn [acc plan]
          (loop [guard-position (guard-position plan)
                 [dir-x dir-y] [0 -1]
                 visited #{}]
            (let [[next-x next-y] (next-position guard-position [dir-x dir-y])]
              (if (out-of-plan? plan [next-x next-y])
                acc
                (if (= "#" (get-in plan [next-y next-x]))
                  (if (contains? visited [guard-position [dir-x dir-y]])
                    (inc acc)
                    (recur guard-position
                           [(- dir-y) dir-x]
                           (conj visited [guard-position [dir-x dir-y]])))
                  (recur [next-x next-y] [dir-x dir-y] visited))))))

        0)
       (time)))
