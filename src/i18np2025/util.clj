(ns i18np2025.util
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clj-http.client :as client])
  (:import [java.io IOException]))

;;; parsing stuff

(defn parse-numbers
  "Parses a string of longs (or a list of strings of numbers, i.e. you
  can omit the `map` call).
  Input: \"n n ...\" or (\"n n ...\" \"...\") ; string (or seq of) of longs
  Output: (n n ...) or ((n ...) ...) ; list (or list of lists) of longs
  Argument `sep` is the regex to use to split numbers within strings,
  default is spaces (one or more).
  Argument `opts` is vector with options:
  - :vec -> produce vectors instead of lists"
  ([in]
   (parse-numbers #" +" in))
  ([what in]
   (if (vector? what)
     (parse-numbers #" " what in)
     (parse-numbers what [:std] in)))
  ([sep opts in]
   (if (coll? in)
     (let [map-fn (if (some #{:vec} opts) mapv map)]
       (->> in
            (map #(str/split % sep))
            (map-fn (fn[x] (map-fn #(parse-long %) x)))))
     (first (parse-numbers sep opts (hash-set in))))))

(comment
  (parse-numbers "1 2 3")
  (parse-numbers #"," "1,2,3")
  (parse-numbers [:vec] "1 2 3")
  (parse-numbers #":" [:vec] "1:2:3")
  (parse-numbers ["1 2 3" "4 5 6"])
  (parse-numbers #"," ["1,2,3" "4,5,6"])
  (parse-numbers [:vec] ["1 2 3" "4 5 6"])
  (parse-numbers #":" [:vec] (list "1:2:3" "4:5:6"))
)

(defn parse-area
  "Parses an area into a map {[r c] <c>, ...}.
  Input is expected to be lines (i.e. a seq of strings).
  Options map (which is optional) can contain:
    :map-chars {<map of chars to some value to put into area map>}
    :ignore-chars #{<set of chars to ignore>}
    :invert? <boolean to get xy instead of rc semantics>
  Example: (parse-area {:ignore-chars \\.} [\"#..\" \"#..\" \".X.\"])
  Result: {[0 0] \\#, [1 0] \\#, [2 1] \\X}"
  ([lines]
   (parse-area nil lines))
  ([{:keys [map-chars ignore-chars invert?]
     :or {map-chars identity}} lines]
   (reduce-kv
     (fn[area row line]
       (reduce-kv
         (fn[area col ch]
           (if (and ignore-chars (ignore-chars ch))
             area
             (assoc area
                    (if invert? [col row] [row col])
                    (map-chars ch))))
         area
         (vec line)))
     {}
     (vec lines))))

(comment
  (parse-area {:map-chars {\S :start \# :wall \E :end}
               :ignore-chars #{\.}}
              '("S..." "##.." "###." "###E"))
)

;;; getting the input

(def *session-cookie-global*
  (str (System/getProperty "user.home") "/.i18np-session-cookie.txt"))

(def *session-cookie-local*
  "var/session-cookie.txt")

(defn get-url
  [day]
  (str "https://i18n-puzzles.com/puzzle/" day "/input"))

(defn get-session-cookie-value
  []
  (cond
    (.exists (io/file *session-cookie-global*)) (slurp *session-cookie-global*)
    (.exists (io/file *session-cookie-local*)) (slurp *session-cookie-local*)))

(defmulti get-input
  "Returns the input for the given day. Pass either a long for the day
  or a namespace object which ends in the day-number (will be extracted).
  Example using long: (get-input 5)
  Example in namespace `aoc.day05`: (get-input *ns*)"
  class)

(defmethod get-input java.lang.Long [day]
  (io/make-parents "var/dummy") ; ignore return of make-parents
  (let [filename (str "var/in-" day ".txt")]
    (when-not (-> filename io/file .exists)
      (if-let [cookie (get-session-cookie-value)]
        (spit filename
              (try
                (:body
                  (client/get (get-url day)
                              {:cookies {"sessionid" {:value cookie}}}))
                (catch Exception e
                  (throw (IOException. (str "Error while fetching " (get-url day)) e)))))
        (throw (IllegalStateException. (str "Cannot http/get input: Missing session string (" *session-cookie-global* " or " *session-cookie-local* ")")))))
    (slurp filename)))

(defmethod get-input clojure.lang.Namespace [ns]
  (get-input (parse-long (#(subs % (- (count %) 2)) (str ns)))))

;;; collections functions

(defn conj-vec
  "Like `conj` but creates a vector if coll is nil."
  [coll x]
  (if coll
    (conj coll x)
    [x]))

(defn conj-set
  "Like `conj` but creates a set if coll is nil."
  [coll x]
  (if coll
    (conj coll x)
    #{x}))

;;; math functions

(defn gcd
  "Return greatest common divisor of a and b."
  [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm
  "Returns lowest common multiple of a and b.
  Note: This method is not tested with negative numbers."
  [a b]
  (/ (Math/abs (* a b)) (gcd a b)))

;;; plotting stuff

(defn plot-maze
  "Arguments:
  - [height width]: size of the maze (from 0, incl. to height/width, excl.)
  - maze: fn [[r c]] to return the to value to be printed at row/col
  - options map:
    * `:fmt` fn[v], optional, to format the maze value before printing
    * `:out` file(name), optional, to print the maze to instead of stdout
    * `:inv?` optional, to invert the coordinates (e.g. xy instead rc)
  Returns the maze itself again."
  [[height width]
   maze
   & {:keys [fmt out inv?]
      :or {fmt (fnil identity \ )}
      :as opts}]
  (if out
    (spit out
          (with-out-str
            (plot-maze [height width] maze (dissoc opts :out))))
    (doseq [r (range height)
            c (range width)]
      (if (and (pos? r) (zero? c))
        (println))
      (print (fmt (maze (if inv? [c r] [r c]))))))
  maze)
