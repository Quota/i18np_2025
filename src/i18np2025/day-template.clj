(ns i18np2025.day00
  (:require
    [clojure.string :as str]
    [i18np2025.util :as util]))

;;; day 00: template

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)))

(defn solve
  []
  (->> (parse-input)
       count))
