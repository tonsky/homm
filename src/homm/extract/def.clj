(ns homm.extract.def
  (:require
    [clojure.java.io :as io]
    [clojure.math :as math]
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

;; DEF file format
;; 
;;          f  r  a  m  e  s
;;    ┌────────┐
;;    │        │
;;  b │        │
;;  l ├────────┼────────┬─────────┐
;;  o │        │        │         │
;;  c │        │        │         │
;;  k ├────────┼────────┼─────────┘
;;  s │        │        │
;;    │        │        │
;;    └────────┴────────┘
;;  
;;          f u l l   w i d t h    
;;    ┌───────────────────────────┐
;;  f │   left                    │
;;  u │  t┌──────────────────┐    │
;;  l │  o│ h   w i d t h    │    │
;;  l │  p│ e                │    │
;;    │   │ i                │    │
;;  h │   │ g                │    │
;;  e │   │ h                │    │
;;  i │   │ t                │    │
;;  g │   └──────────────────┘    │
;;  h │                           │
;;  t │                           │
;;    └───────────────────────────┘
;;
;;   offset:0   length:4 :: int :: type
;;   offset:4   length:4 :: int :: width
;;   offset:8   length:4 :: int :: height
;;   offset:12  length:4 :: int :: number of blocks (B)
;;   offset:16  length:(3 * 256) :: bytes :: palette (r, g, b) * 256
;;   offset:784 length:(B * ?)   :: bytes :: block info
;;
;; Block info
;;   offset:+0  length:4 :: int :: block id
;;   offset:+4  length:4 :: int :: number of frames (F)
;;   offset:+8  length:4 :: int :: ?
;;   offset:+12 length:4 :: int :: ?
;;   offset:+16 length:(F * 13) :: strings :: frame names
;;   offset:+(16 + F * 13) length:(F * 4) :: ints :: frame offsets
;;
;; Frame
;;   offset:+0  length:4 :: int :: size
;;   offset:+4  length:4 :: int :: format 0 | 1 | 2 |3
;;   offset:+8  length:4 :: int :: full-width
;;   offset:+12 length:4 :: int :: full-height
;;   offset:+16 length:4 :: int :: width (W)
;;   offset:+20 length:4 :: int :: height (H)
;;   offset:+24 length:4 :: int :: left-margin
;;   offset:+28 length:4 :: int :: top-margin
;;   offset:+32 lenght:? :: bytes :: pixel data
;;
;; Frame pixel data (format 0)
;;   offset:+32 length:(W * H) :: bytes :: color indices from palette
;;
;; Frame pixel data (format 1)
;;   offset:+32 length:(4 * H) :: ints :: line relative offsets (Rs)
;; Repeat until total length of W is accumulated
;;   offset:+(32 + R)     length:1 :: byte :: code (C)
;;   offset:+(32 + R + 1) length:1 :: byte :: lenght - 1 (L)
;; if C == 255
;;   offset:+(32 + R + 2) length:(L + 1) :: bytes :: color indices from palette
;; else
;;   offset:+(32 + R + 2) length:0 :: nothing :: repeat color C times (L + 1)
;;
;; Frame pixel data (format 2)
;;   offset:+32 length:(2 * H) :: shorts :: line relative offsets (Rs)
;; Repeat until total length of W is accumulated
;;   offset:+(32 + R)     length:1 :: byte :: segment (S) == 3 bits code (C) + 5 bits length - 1 (L)
;; if C == 7
;;   offset:+(32 + R + 1) length:(L + 1) :: bytes :: color indices from palette
;; else
;;   offset:+(32 + R + 1) length:0 :: nothing :: repeat color C times (L + 1)
;;
;; Frame pixel data (format 3)
;;   offset:+32 length:(2 * H * W / 32) :: shorts :: fragment relative offsets (Rs)
;; Repeat until total length of 32 is accumulated
;;   offset:+(32 + R)     length:1 :: byte :: segment (S) == 3 bits code (C) + 5 bits length - 1 (L)
;; if C == 7
;;   offset:+(32 + R + 1) length:(L + 1) :: bytes :: color indices from palette
;; else
;;   offset:+(32 + R + 1) length:0 :: nothing :: repeat color C times (L + 1)

(defn extract-frame-info [^ByteBuffer buf idx name offset]
  (bb/with-position buf
    (bb/position buf (int (+ offset 16)))
    (let [width  (bb/get-int buf)
          height (bb/get-int buf)
          left   (bb/get-int buf)
          top    (bb/get-int buf)]
      {:idx    idx
       :name   name
       :offset offset
       :left   left
       :top    top
       :right  (+ left width)
       :bottom (+ top height)
       :width  width
       :height height})))

(defn extract-block-info [^ByteBuffer buf]
  (let [id         (.getInt buf)
        frames-len (.getInt buf)
        _          (.getInt buf)
        _          (.getInt buf)
        names      (core/repeatedlyv frames-len #(bb/get-string buf 13))
        offsets    (core/repeatedlyv frames-len #(.getInt buf))]
    {:id      id
     :names   names
     :offsets offsets
     :frames  (mapv #(extract-frame-info buf %1 %2 %3) (range) names offsets)}))

(defn extract-frame [^ByteBuffer buf set-color frame-offset]
  (.position buf (int frame-offset))
  (let [_size        (.getInt buf)
        format       (.getInt buf)
        _full-width  (.getInt buf)
        _full-height (.getInt buf)
        width        (.getInt buf)
        height       (.getInt buf)
        left-margin  (.getInt buf)
        top-margin   (.getInt buf)]
    ; (prn format full-width full-height width height left-margin top-margin)
    (case format
      0
      (doseq [y (range 0 height)
              x (range 0 width)]
        (set-color x y (bb/get-byte buf)))

      1
      (core/doindexed [y rel-offset (bb/get-ints buf height)]
        (bb/position buf (int (+ frame-offset 32 rel-offset)))
        (loop [total-length 0]
          (let [code   (bb/get-unsigned-byte buf)
                length (long (inc (bb/get-unsigned-byte buf)))]
            (if (= code 255)
              ;; raw data
              (dotimes [dx length]
                (set-color (+ total-length dx) y (bb/get-byte buf)))
              ;; rle encoding
              (dotimes [dx length]
                (set-color (+ total-length dx) y code)))
            (when (< (+ total-length length) width)
              (recur (+ total-length length))))))
      
      2
      (doseq [[y rel-offset] (core/forv [y (range 0 height)]
                               [y (bb/get-unsigned-short buf)])]
        (bb/position buf (int (+ frame-offset 32 rel-offset)))
        (loop [total-length 0]
          (let [segment (bb/get-unsigned-byte buf)
                code    (unsigned-bit-shift-right segment 5)
                length  (inc (bit-and segment 0x1F))]
            (if (= 7 code)
              ; raw data
              (dotimes [dx length]
                (set-color (+ total-length dx) y (.get buf)))
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
          (let [segment (bb/get-unsigned-byte buf)
                code    (unsigned-bit-shift-right segment 5)
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

(defn extract-info [^ByteBuffer buf]
  (.rewind buf)
  (let [type        (.getInt buf)
        orig-width  (.getInt buf)
        orig-height (.getInt buf)
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
        blocks       (core/repeatedlyv blocks-len #(extract-block-info buf))
        frames       (->> blocks
                       (mapcat #(core/zip (:names %) (:offsets %)))
                       distinct
                       (sort-by first)
                       (mapv (fn [idx [name offset]]
                               (extract-frame-info buf idx name offset)) (range)))
        min-left     (transduce (map :left)   min orig-width  frames)
        min-top      (transduce (map :top)    min orig-height frames)
        max-right    (transduce (map :right)  max 0           frames)
        max-bottom   (transduce (map :bottom) max 0           frames)
        width        (- max-right min-left)
        height       (- max-bottom min-top)
        [cols rows]  (core/best-fit 1.61803 width height (count frames))
        full-width   (* width cols)
        full-height  (* height rows)
        bgra         ^bytes (make-array Byte/TYPE (* full-width full-height 4))
        set-color-fn (fn [x y left top]
                       (fn [frame-x frame-y color]
                         (let [[r g b a] (get-color color)
                               x'        (-> (* x width) (+ frame-x) (+ left) (- min-left))
                               y'        (-> (* y height) (+ frame-y) (+ top) (- min-top))
                               i         (-> y' (* full-width) (+ x') (* 4))]
                           (aset bgra (+ i 0) (byte b))
                           (aset bgra (+ i 1) (byte g))
                           (aset bgra (+ i 2) (byte r))
                           (aset bgra (+ i 3) (byte a)))))
        frame-idx    (into {} (map (juxt :name :idx) frames))
        frames'      (core/forv [[y row]   (core/zip (range) (partition-all cols frames))
                                 [x frame] (core/zip (range) row)]
                       (let [{:keys [idx name offset left top]} frame]
                         (extract-frame buf (set-color-fn x y left top) offset)
                         {:name name
                          :left (* x width)
                          :top  (* y height)}))
        blocks       (into (sorted-map)
                       (for [block blocks]
                         [(:id block) (mapv frame-idx (:names block))]))]
    {:type        type
     :width       width
     :height      height
     :orig-width  orig-width
     :orig-height orig-height
     :margin-left min-left
     :margin-top  min-top
     :blocks      blocks
     :frames      frames'
     :image       (image/from-bgra (bb/wrap bgra) full-width full-height)}))

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
     (print (str "Extracting " from "..."))
     (flush)
     (Files/createDirectories (core/path to) (make-array FileAttribute 0))
     (with-open [fc (FileChannel/open (core/path from) (make-array OpenOption 0))]
       (let [t0    (System/currentTimeMillis)
             buf   (.map fc FileChannel$MapMode/READ_ONLY 0 (.size fc))
             _     (.order buf ByteOrder/LITTLE_ENDIAN)
             bytes (extract-info buf)]
         (bb/write (image/to-png bytes) (.getPath png))
         (println (- (System/currentTimeMillis) t0) "ms"))))))

(defn format-meta [info]
  (as-> info %
    (dissoc % :image)
    (with-out-str
      (clojure.pprint/pprint %))
    (.getBytes ^String % "UTF-8")
    (bb/wrap %)))

(defn extract-lod [lod name]
  (let [file          ((requiring-resolve 'homm.extract/file-buf) lod name)
        info          (extract-info file)
        [path base _] (core/split-file lod)
        dir           (str path " Extracted/" base)
        [_ base _]    (core/split-file name)
        png           (str dir "/" base ".png")
        edn           (str dir "/" base ".edn")]
    (bb/write (image/to-png (:image info)) png)
    (bb/write (format-meta info) edn)))

(comment    
  ; type 1
  (extract-lod "resources/Data/H3sprite.lod" "iam000.def")
  
  ; type 1 large
  (extract-lod "resources/Data/H3sprite.lod" "Czealt.def")
  (extract-lod "resources/Data/H3sprite.lod" "CZOMBI.def")
  
  ; type 2
  (extract-lod "resources/Data/H3sprite.lod" "Clrrvr.def")
  
  ; type 3
  (extract-lod "resources/Data/H3sprite.lod" "AB01_.def"))
