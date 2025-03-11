(ns i18np2025.day03
  (:require
    [clojure.string :as str]
    [i18np2025.util :as util]))

;;; day 03: Unicode passwords

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)))


(defn check-password
  [pwd]
  (and (<= 4 (.length pwd) 12)
       (every? #(some % pwd)
               [Character/isDigit
                Character/isUpperCase
                Character/isLowerCase
                #(-> % long (> 127))])))

(defn solve
  []
  (->> (parse-input)
       (filter check-password)
       count))
