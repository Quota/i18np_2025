(ns i18np2025.day01
  (:require
    [clojure.string :as str]
    [i18np2025.util :as util]))

;;; day 01: Length limits on messaging platforms

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)))

(defn fits-sms?
  [line]
  (-> line
      (.getBytes "UTF-8")
      count
      (<= 160)))

(defn fits-tweet?
  [line]
  (-> line
      count
      (<= 140)))

(defn calc-total-costs
  [sms? tweet?]
  (cond
    (and sms? tweet?) 13
    sms? 11
    tweet? 7
    :else 0))

(comment
  (calc-total-costs false false)
  (calc-total-costs true false)
  (calc-total-costs false true)
  (calc-total-costs true true)
  )

(defn calculate-costs
  [line]
  (let [sms? (fits-sms? line)
        tweet? (fits-tweet? line)]
    (calc-total-costs sms? tweet?)))

(defn solve
  []
  (->> (parse-input)
       (map calculate-costs)
       (reduce +))
