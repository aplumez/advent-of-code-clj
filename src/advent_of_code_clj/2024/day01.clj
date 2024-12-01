(ns advent-of-code-clj.2024.day01
  (:require
   [advent-of-code-clj.core :refer [puzzle]]
   [clojure.string :as string]))

(def input (-> (puzzle 2024 1)
               (string/split-lines)))
