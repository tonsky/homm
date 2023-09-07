(ns homm.core.byte-buffer
  (:require
    [homm.core :as core])
  (:import
    [java.nio ByteBuffer ByteOrder MappedByteBuffer]
    [java.nio.channels FileChannel FileChannel$MapMode]
    [java.nio.file OpenOption StandardOpenOption]))

(set! *warn-on-reflection* true)

(defn ^ByteBuffer allocate [^long n]
  (doto (ByteBuffer/allocate n)
    (.order ByteOrder/LITTLE_ENDIAN)))

(defn ^ByteBuffer wrap [^bytes bytes]
  (doto (ByteBuffer/wrap bytes)
    (.order ByteOrder/LITTLE_ENDIAN)))

(defn mmap ^MappedByteBuffer [path]
  (with-open [fc (FileChannel/open (core/path path) (make-array OpenOption 0))]
    (doto (.map fc FileChannel$MapMode/READ_ONLY 0 (.size fc))
      (.order ByteOrder/LITTLE_ENDIAN))))

(defmacro with-mapped [[buf path] & body]
  `(let [~buf (mmap ~path)]
     ~@body))

(defn write [^ByteBuffer buf path]
  (with-open [fc (FileChannel/open (core/path path)
                   (into-array OpenOption [StandardOpenOption/CREATE
                                           StandardOpenOption/TRUNCATE_EXISTING
                                           StandardOpenOption/WRITE]))]
    (.rewind buf)
    (.write fc buf)))

(defn array [^ByteBuffer buf]
  (if (.hasArray buf)
    (.array buf)
    (let [_ (.rewind buf)
          bytes ^bytes (make-array Byte/TYPE (.remaining buf))]
      (.get buf bytes)
      bytes)))

(defn position
  (^long [^ByteBuffer buf]
    (.position buf))
  (^ByteBuffer [^ByteBuffer buf ^long pos]
    (.position buf pos)))

(defn ^ByteBuffer advance [^ByteBuffer buf ^long offset]
  (position buf (+ (position buf) offset)))

(defmacro with-position [buf & body]
  `(let [^ByteBuffer buf# ~buf
         pos# (position buf#)
         res# (do ~@body)]
     (position buf# pos#)
     res#))

(defn slice ^ByteBuffer [^ByteBuffer buf idx len]
  (doto (.slice buf (int idx) (int len))
    (.order ByteOrder/LITTLE_ENDIAN)))

(defn byte->long [^long n]
  (assert (<= -128 n 127) (str "Expected -128..127, got: " n))
  (-> n (+ 256) (mod 256)))

(defn long->byte [^long n]
  (assert (<= 0 n 255) (str "Expected 0..255, got: " n))
  (if (>= n 128)
    (- n 256)
    n))

(defn get-byte
  ([^ByteBuffer buf]
   (.get buf))
  ([^ByteBuffer buf ^long pos]
   (.get buf pos)))

(defn put-byte
  ([^ByteBuffer buf ^long b]
   (.put buf (byte b)))
  ([^ByteBuffer buf ^long pos ^long b]
   (.put buf pos (byte b))))

(defn get-unsigned-byte
  ([^ByteBuffer buf]
   (-> (.get buf) (+ 256) (mod 256)))
  ([^ByteBuffer buf ^long pos]
   (-> (.get buf pos) (+ 256) (mod 256))))

(defn put-unsigned-byte
  ([^ByteBuffer buf ^long b]
   (.put buf (byte (if (>= b 128) (- b 256) b))))
  ([^ByteBuffer buf ^long pos ^long b]
   (.put buf pos (byte (if (>= b 128) (- b 256) b)))))

(defn get-unsigned-short
  ([^ByteBuffer buf]
   (-> (.getShort buf) (+ 65536) (mod 65536)))
  ([^ByteBuffer buf ^long pos]
   (-> (.getShort buf pos) (+ 65536) (mod 65536))))

(defn get-int
  (^long [^ByteBuffer buf]
    (.getInt buf))
  (^long [^ByteBuffer buf ^long pos]
    (.getInt buf pos)))

(defn get-ints [^ByteBuffer buf ^long cnt]
  (let [res (java.util.ArrayList. (int cnt))]
    (dotimes [i cnt]
      (.add res (.getInt buf)))
    res))

(defn ^String get-string
  ([^ByteBuffer buf size]
   (get-string buf (.position buf) size))
  ([^ByteBuffer buf idx size]
   (let [arr (byte-array size)
         _   (.get buf idx arr)
         len (some #(when (= (byte 0) (aget arr %)) %) (range size))]
     (advance buf size)
     (String. arr 0 (int len)))))
