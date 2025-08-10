(ns log-levels
  (:require [clojure.string :as str]))

(def regex #"^\[(ERROR|INFO|WARNING)\]:\s(.*)")

(defn parse-line
  [s]
  (some->> 
    s
    (re-find regex)
    (drop 1)))

(defn message
  "Takes a string representing a log line
   and returns its message with whitespace trimmed."
  [s]
  (-> s parse-line second str/trim))

(defn log-level
  "Takes a string representing a log line
   and returns its level in lower-case."
  [s]
  (-> s parse-line first str/lower-case))

(defn reformat
  "Takes a string representing a log line and formats it
   with the message first and the log level in parentheses."
  [s]
  (let [log-line (parse-line s)
        template "%s (%s)"
        log-level (str/lower-case (first log-line))
        log-message (str/trim (second log-line))]
    (format template log-message log-level)
  ))
