(ns homm.core
  (:require
    [clojure.string :as str])
  (:import
    [io.github.humbleui.skija ColorAlphaType EncodedImageFormat Image ImageInfo]
    [java.nio ByteBuffer ByteOrder]
    [java.nio.file Files OpenOption Path]))

(set! *warn-on-reflection* true)

(defn path ^Path [s & ss]
  (Path/of s (into-array String ss)))

(defn zip [& xss]
  (apply map vector xss))

(defmacro forv [seq-exprs & body-exprs]
  `(vec
     (for ~seq-exprs (do ~@body-exprs))))

(defmacro form [seq-exprs & body-exprs]
  `(into {}
     (for ~seq-exprs (do ~@body-exprs))))

(defn repeatedlyv
  ([f] (vec (repeatedly f)))
  ([n f] (vec (repeatedly n f))))

(defmacro doindexed [[i x xs] & body]
  `(let [xs# ~xs]
     (dotimes [~i (count xs#)]
       (let [~x (nth xs# ~i)]
         ~@body))))

(defn split-file [n]
  (let [[path name] (if-some [i (str/last-index-of n "/")]
                      [(subs n 0 i) (subs n (inc i))]
                      [nil n])
        [base ext]  (if-some [i (str/last-index-of name ".")]
                      [(subs name 0 i) (subs name (inc i))]
                      [name nil])]
    [path base ext]))

(defn best-fit
  "Given target ratio, WxH of every item and number of items,
   tries to arrange them in a rectangular grid so that resulting
   table has ratio closest to target.
   
   Returns [cols rows]"
  [target w h cnt]
  (->>
    (for [cols (range 1 (inc cnt))
          :let [rows  (-> cnt (- 1) (quot cols) (+ 1))
                ratio (/ (* w cols) (* h rows))
                delta (abs (- target ratio))]]
      [delta cols rows])
    (sort-by first)
    first
    next
    vec))
