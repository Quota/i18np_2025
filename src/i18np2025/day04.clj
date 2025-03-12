(ns i18np2025.day04
  (:require
    [clojure.string :as str]
    [i18np2025.util :as util])
  (:import
    [java.time Duration LocalDateTime ZoneId]
    [java.time.format DateTimeFormatter]))

;;; day 04: A trip around the world

(def date-time-format
  "Date-time-format according to the assignment."
  (DateTimeFormatter/ofPattern "MMM d, yyyy, HH:mm"))

(defn parse-line
  "Parses a line like \"...:  Europe/Amsterdam   Jan 12, 2020, 11:00\"
  returning a `ZonedDateTime`. Whitespaces are ignored."
  [line]
  (let [[_ zone-str date-time-str] (re-find #".*:\s+(\S+)\s+(.*)$" line)]
    (-> date-time-str
        (LocalDateTime/parse date-time-format)
        (.atZone (ZoneId/of zone-str)))))

(defn parse-input
  "Returns input as seq of `ZonedDateTime` objects. Blank lines are skipped."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)
       (remove str/blank?)
       (map parse-line)))

(defn calc-diff-minutes
  "Returns the difference between the two given dates, in minutes."
  [start-date-time end-date-time]
  (-> (Duration/between start-date-time end-date-time)
      (.getSeconds)
      (/ 60)))

(defn solve
  []
  (->> (parse-input)
       (partition 2)
       (map #(apply calc-diff-minutes %))
       (apply +)))
