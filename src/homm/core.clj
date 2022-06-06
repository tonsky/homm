(ns homm.core
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
