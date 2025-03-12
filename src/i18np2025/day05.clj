(ns i18np2025.day05
  (:require
    [clojure.string :as str]
    [i18np2025.util :as util]))

;;; day 05: Don't step in it...

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (map #(.codePoints %))
       (map #(.toArray %))
       (mapv vec)))

(defn solve
  []
  (let [park (parse-input)
        width (-> park first count)
        height (-> park count)
        sh*t (.codePointAt "💩" 0)]
    (->> (for [row (range height)]
           [row (-> row (* 2) (mod width))])
         (map #(get-in park %))
         (filter #{sh*t})
         count)))
