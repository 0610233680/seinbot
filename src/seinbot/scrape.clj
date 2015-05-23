(ns seinbot.scrape
  (:refer-clojure :exclude [vector])
  (:require [net.cgrand.enlive-html :as html]
            [mutils.fn.compose :as c]
            [clj-tuple :refer [vector]]
            [clojure.java.io :refer [reader]]
            [clojure.string :as string])
  (:import [java.net URL]))

(defn lazy-mapcat [f coll]
  (for [x coll, fx (f x)] fx))

(defn fetch-url [url]
  (html/html-resource (URL. url)))

(def speaker
  (comp string/lower-case
        first
        #(string/split % #":")))

(defn line? [characters text]
  (->> text (speaker) (characters)))

(def lead-characters
  #{"jerry" "george" "elaine" "kramer"})

(def lead-line? (partial line? lead-characters))

(def selection->lead-lines
  (c/comp' (filter (c/and-comp lead-line? (complement empty?)))
           (lazy-mapcat #(string/split % #"[\n\t]"))
           (map html/text)))

(defn get-actions [^String line]
  (let [chars (vec line)]
    (loop [[char & tail] chars
           i 0
           paren-indices (vector)]
      (case char
        nil
        (map (fn [[start end]] (.substring line start (inc end))) paren-indices)
        \(
        (recur tail (inc i) (conj paren-indices (vector i)))
        \)
        (let [last (dec (count paren-indices))]
          (recur tail (inc i) (update-in paren-indices [last] conj i)))
        (recur tail (inc i) paren-indices)))))

(defn line-details [line]
  (let [actions (get-actions line)
        no-actions (reduce  #(string/replace %1 %2 "") line actions)
        speech-only (string/join \: (rest (string/split no-actions #":")))]
    {:raw line
     :speaker (speaker line)
     :actions (vec actions)
     :speech speech-only}))

(def ^:const base-url
  "http://www.seinology.com/scripts/script-%s.shtml")

(defn episode->string [episode]
  (case episode
    (34 35) "35"
    (82 83) "82and83"
    (100 101 177 178) nil
    (179 180) "179and180"
    (if (< episode 10) (str "0" episode) (str episode))))

(defn raw-lines
  ([] (raw-lines 1 180))
  ([episode]
     (when-let [ep-str (episode->string episode)]
       (let [url (format base-url ep-str)
             page (fetch-url url)]
         (selection->lead-lines (html/select page [:p])))))
  ([start end]
     (distinct (lazy-mapcat raw-lines (range start (inc end))))))

(def downloaded-lines
  (comp line-seq #(reader "lead-character-lines.txt")))
