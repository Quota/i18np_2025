(ns i18np2025.day08
  (:require
    [clojure.string :as str]
    [i18np2025.util :as util])
  (:import
    [java.time Duration Instant LocalDateTime ZoneId]
    [java.time.format DateTimeFormatter]
    [java.text Normalizer Normalizer$Form]))

;;; day 08: Unicode passwords redux

(defn normalize-first-char
  "Normalizes (NFD) a string and returns the first character as lower-case."
  [s]
  (-> s
      (Normalizer/normalize Normalizer$Form/NFD)
      (.charAt 0)
      (Character/toLowerCase)))

(defn normalize-string
  "Normalizes (NFD) a string and returns all characters in a vec."
  [s]
  (mapv normalize-first-char
        (for [i (range (count s))]
          (.substring s i (inc i)))))

(defn parse-input
  "Returns input as seq of lines."
  []
  (->> (util/get-input *ns*)
       (str/split-lines)))

(defn vowel?
  "Returns if c is a vowel (a, e, i, o, u)."
  [c]
  (#{\a \e \i \o \u} c))

(defn consonant?
  "Returns if c is a consonant (a letter but not a vowel)."
  [c]
  (and (Character/isLetter c)
       (not (vowel? c))))

(defn recurring-letters?
  "Returns if there are recurring letters in a string."
  [s]
  (->> (frequencies s)
       (map second)
       (some #(> % 1))))

(defn check-password
  "Verifys the password according to this assignments rules."
  [pwd]
  (let [norm-pwd (normalize-string pwd)]
    (and (<= 4 (.length pwd) 12)
         (some Character/isDigit pwd)
         (some vowel? norm-pwd)
         (some consonant? norm-pwd)
         (not (recurring-letters? norm-pwd)))))

(defn solve
  []
  (->> (parse-input)
       (filter check-password)
       count))
