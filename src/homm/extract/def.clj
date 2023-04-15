(ns homm.extract.def
  (:require
    [clojure.java.io :as io]
    [clojure.pprint :as pprint]
    [clojure.string :as str]
    [homm.core :as core]
    [homm.core.byte-buffer :as bb]
    [homm.core.image :as image])
  (:import
    [io.github.humbleui.skija ColorAlphaType EncodedImageFormat Image ImageInfo]
    [java.nio ByteBuffer ByteOrder]
    [java.nio.channels FileChannel FileChannel$MapMode]
    [java.nio.file Files OpenOption Path]
    [java.nio.file.attribute FileAttribute]
    [java.util.zip Inflater]))

(set! *warn-on-reflection* true)

(defn extract-block-info [^ByteBuffer buf]
  (let [id         (.getInt buf)
        frames-len (.getInt buf)
        _          (.getInt buf)
        _          (.getInt buf)]
    {:id         id
     :frames-len frames-len
     :names      (core/repeatedlyv frames-len #(bb/get-string buf 13))
     :offsets    (core/repeatedlyv frames-len #(.getInt buf))}))

(defn extract-frame [^ByteBuffer buf set-color frame-offset]
  (.position buf (int frame-offset))
  (let [_size       (.getInt buf)
        format      (.getInt buf)
        full-width  (.getInt buf)
        full-height (.getInt buf)
        width       (.getInt buf)
        height      (.getInt buf)
        left-margin (.getInt buf)
        top-margin  (.getInt buf)]
    ; (prn format full-width full-height width height left-margin top-margin)
    (case format
      0
      (doseq [y (range 0 height)
              x (range 0 width)]
        (set-color x y (.get buf)))

      1
      (doseq [[y rel-offset] (core/forv [y (range 0 height)]
                               [y (.getInt buf)])]
        (.position buf (int (+ frame-offset 32 rel-offset)))
        (loop [total-length 0]
          (let [code   (.get buf)
                length (long (inc (bb/get-byte buf)))]
            (if (= code -1)
              ;; raw data
              (dotimes [dx length]
                (set-color (+ total-length dx) y (.get buf)))
              ;; rle encoding
              (dotimes [dx length]
                (set-color (+ total-length dx) y code)))
            (when (< (+ total-length length) width)
              (recur (+ total-length length))))))
      
      2
      (doseq [[y rel-offset] (core/forv [y (range 0 height)]
                               [y (bb/get-unsigned-short buf)])]
        (.position buf (int (+ frame-offset 32 rel-offset)))
        (loop [total-length 0]
          (let [segment (bb/get-byte buf)
                code    (bit-shift-right segment 5)
                length  (inc (bit-and segment 0x1F))]
            (if (= 7 code)
              ; raw data
              (dotimes [dx length]
                (set-color (+ total-length dx) y (bb/get-byte buf)))
              ; rle encoding
              (dotimes [dx length]
                (set-color (+ total-length dx) y code)))
            (when (< (+ total-length length) width)
              (recur (+ total-length length))))))
      
      3 ; each row is split into 32 byte long blocks which are individually encoded
      ; two bytes store the offset for each block per line 
      (doseq [[x y rel-offset] (core/forv [y (range 0 height)
                                           x (range 0 width 32)]
                                 [x y (bb/get-unsigned-short buf)])]
        (.position buf (int (+ frame-offset 32 rel-offset)))
        (loop [total-length 0]
          (let [segment (bb/get-byte buf)
                code    (bit-shift-right segment 5)
                length  (inc (bit-and segment 0x1F))]
            (if (= 7 code)
              ; raw data
              (dotimes [dx length]
                (set-color (+ x total-length dx) y (bb/get-byte buf)))
              ; rle encoding
              (dotimes [dx length]
                (set-color (+ x total-length dx) y code)))
            (when (< (+ total-length length) 32)
              (recur (+ total-length length)))))))))

(defn extract
  ([dir]
   (doseq [name  (sort (.list (io/file "resources/Data" dir)))
           :when (str/ends-with? name ".def")
           :when (not= "SGTWMTA.def" name)
           :when (not= "SGTWMTB.def" name)]
     (extract (str "resources/Data/" dir "/" name) (str "resources/Data Extracted/" dir))))
  ([from to]
   (let [basename (-> from io/file .getName (->> (re-matches #"(.*)\.def")) second)
         png      (io/file to (str basename ".png"))]
     (when-not (.exists png)    
       (print (str "Extracting " from "..."))
       (flush)
       (Files/createDirectories (core/path to) (make-array FileAttribute 0))
       (with-open [fc (FileChannel/open (core/path from) (make-array OpenOption 0))]
         (let [t0       (System/currentTimeMillis)
               buf         (.map fc FileChannel$MapMode/READ_ONLY 0 (.size fc))
               _           (.order buf ByteOrder/LITTLE_ENDIAN)
               type        (.getInt buf)
               width       (.getInt buf)
               height      (.getInt buf)
               blocks-len  (.getInt buf)
               _           (bb/advance buf (* 3 256)) ;; palette
               get-color   (fn [color]
                             (let [color (long (bb/byte->long color))]
                               (case color
                                 0 [0 0 0 0]    ; full transparency
                                 1 [0 0 0 64]   ; shadow border
                                 4 [0 0 0 -128] ; shadow body
                                 5 [0 0 0 0]    ; selection highlight, treat as full transparency
                                 6 [0 0 0 -128] ; shadow body below selection, treat as shadow body
                                 7 [0 0 0 64]   ; shadow border below selection, treat as shadow border
                                 (let [i (+ 16 (* color 3))]
                                   [(.get buf (+ i 0))
                                    (.get buf (+ i 1))
                                    (.get buf (+ i 2))
                                    -1]))))
               blocks      (vec (repeatedly blocks-len #(extract-block-info buf)))
               frames-len  (reduce max (map :frames-len blocks))
               full-width  (* width frames-len)
               full-height (* height blocks-len)
               bgra        ^bytes (make-array Byte/TYPE (* full-width full-height 4))
               res         {:type type
                            :width  width
                            :height height
                            :blocks
                            (core/forv [[y block] (core/zip (range) blocks)]
                              {:id     y
                               :frames
                               (core/forv [[x name offset] (core/zip (range) (:names block) (:offsets block))
                                           :let [set-color (fn [frame-x frame-y color]
                                                             (let [[r g b a] (get-color color)
                                                                   x'        (-> (* x width) (+ frame-x))
                                                                   y'        (-> (* y height) (+ frame-y))
                                                                   i         (-> y' (* full-width) (+ x') (* 4))]
                                                               (aset bgra (+ i 0) (byte b))
                                                               (aset bgra (+ i 1) (byte g))
                                                               (aset bgra (+ i 2) (byte r))
                                                               (aset bgra (+ i 3) (byte a))))]]
                                 (do
                                   (extract-frame buf set-color offset)
                                   {:id   x
                                    :name name
                                    :left (* x width)
                                    :top  (* y height)}))})}]
           (bb/write (image/to-png full-width full-height bgra) (.toPath png))
           #_(spit (io/file to (str basename ".edn"))
               (with-out-str
                 (pprint/pprint res)))
           (println (- (System/currentTimeMillis) t0) "ms")))))))

(defn -main [& args]
  (extract "H3sprite")
  (extract "H3ab_spr"))
  
(comment
  (require 'heroes.extract.def :reload)
  (extract "H3sprite")
  (extract "H3ab_spr")
  
  (extract "resources/Data/H3sprite/AB01_.def" "resources/Data Extracted/H3sprite")
  (extract "resources/Data/H3sprite/Czealt.def" "resources/Data Extracted/H3sprite")
  (extract "resources/Data/H3sprite/Clrrvr.def" "resources/Data Extracted/H3sprite")
  (extract "resources/Data/H3sprite/ABF01B.def")
  (extract "resources/Data/H3sprite/AVWcycl0.def")
  (extract "resources/Data/H3sprite/AVXmn7i0.def")
  (extract "resources/Data/H3sprite/TBStUp_6.def"))