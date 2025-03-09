(ns i18np2025.day02
  (:require
    [clojure.string :as str]
    [i18np2025.util :as util])
  (:import
    [java.time ZonedDateTime ZoneId]
    [java.time.format DateTimeFormatter]))

;;; day 02: Detecting gravitational waves

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)))

(defn parse-date-to-instant
  [line]
  (-> line
      (ZonedDateTime/parse DateTimeFormatter/ISO_OFFSET_DATE_TIME)
      (.toInstant)))

(defn solve
  []
  (->> (parse-input)
       (map parse-date-to-instant)
       frequencies
       (filter (fn[[k v]] (>= v 4)))
       ffirst
       str
       (#(str/replace % #"^(.*)Z$" "$1+00:00"))))
