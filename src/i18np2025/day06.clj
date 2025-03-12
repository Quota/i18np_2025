(ns i18np2025.day06
  (:require
    [clojure.string :as str]
    [i18np2025.util :as util])
  (:import java.nio.charset.StandardCharsets))

;;; day 06: Mojibake puzzle dictionary

(defn parse-input
  "Returns input as the following map:
  {:words [{:idx <i> :word <w>}] ...] :puzzles [<puzzle-line> ...]}"
  []
  (let [[words [_ & puzzles]]
        (->> (util/get-input *ns*)
             (str/split-lines)
             (split-with seq)) ; split at the empty string
        words (map-indexed (comp #(update % :idx inc)
                                 #(zipmap [:idx :word] %)
                                 vector)
                           words)
        puzzles (map str/trim puzzles)]
    {:words words :puzzles puzzles}))

(defn unscramble
  "Convertes the bytes of the given string to UTF-8 by interpreting
  them as ISO-8859-1."
  [word]
  (-> word
      (.getBytes StandardCharsets/ISO_8859_1)
      (String. StandardCharsets/UTF_8)))

(defn repair-word
  "Returns the argument updated according to the assignments rules.
  I.e. uses `unscramble` to repair every 3rd and 5th word (twice when
  both 3rd and 5th)."
  [{:keys [idx] :as data}]
  (cond
    ; every 3rd AND 5th word -> unscramble twice
    (and (zero? (mod idx 3)) (zero? (mod idx 5)))
    (update data :word #(-> % unscramble unscramble))
    ; every 3rd OR 5th word -> unscramble once
    (or (zero? (mod idx 3)) (zero? (mod idx 5)))
    (update data :word unscramble)
    ; everything else -> it's ok already
    :everything-else-is-already-ok
    data))

(defn solve-puzzle
  "Solves the puzzle using the given words. Returns the original
  pair [i word] of the word fitting the given puzzle line."
  [puzzle words]
  (let [re (re-pattern puzzle)]
    (->> words
         (filter #(re-matches re (:word %)))
         first)))

(defn solve
  []
  (let [{:keys [words puzzles]} (parse-input)
        repaired-words (map repair-word words)]
    (->> puzzles
         (map #(solve-puzzle % repaired-words))
         (map :idx)
         (reduce +))))
