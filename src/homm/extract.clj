(ns homm.extract
  (:require
    [clojure.java.io :as io]
    [clojure.string :as str]
    [homm.core :as core]
    [homm.core.byte-buffer :as bb]
    [homm.core.image :as image])
  (:import
    [java.nio ByteBuffer ByteOrder]
    [java.nio.channels FileChannel FileChannel$MapMode]
    [java.nio.file Files OpenOption Path]
    [java.nio.file.attribute FileAttribute]
    [java.util.zip Inflater]))

(set! *warn-on-reflection* true)

;; Based on https://gitlab.mister-muffin.de/josch/lodextract
;; and https://github.com/vcmi/vcmi/blob/31c9d6e28d2f063c50f9bf4e435410d20337cee1/client/gui/CAnimation.cpp

;; PCX file format (paletted, size = width * height)
;;   offset:0 length:4                             :: int    :: size
;;   offset:4 length:4                             :: int    :: width
;;   offset:8 length:4                             :: int    :: height
;;   offset:12 lenght:(width * height)             :: bytes  :: pixel color indices
;;   offset:(12 + width * height) lenght:(3 * 256) :: bytes  :: palette (r, g, b) * 256

;; PCX file format (RGB, size = 3 * width * height)
;;   offset:0  length:4                    :: int    :: size
;;   offset:4  length:4                    :: int    :: width
;;   offset:8  length:4                    :: int    :: height
;;   offset:12 lenght:(3 * width * height) :: bytes  :: pixel colors (b, g, r)

(defn palette-color [buf ^long color]
  (case color
    0 [0 0 0   0] ;; 100% transparency
    1 [0 0 0  32] ;; 75% shadow border
    2 [0 0 0  64] ;; ???
    3 [0 0 0 128] ;; ???
    4 [0 0 0 128] ;; 50% shadow body
    5 [0 0 0   0] ;; 100% selection highlight
    6 [0 0 0 128] ;; 50% shadow body below selection
    7 [0 0 0  64] ;; 75% shadow border below selection
    [(bb/get-unsigned-byte buf (+ (* color 3) 0))
     (bb/get-unsigned-byte buf (+ (* color 3) 1))
     (bb/get-unsigned-byte buf (+ (* color 3) 2))
     255]))

(defn read-pcx [^ByteBuffer buf]
  (.rewind buf)
  (let [size      (bb/get-int buf)
        width     (bb/get-int buf)
        height    (bb/get-int buf)
        bgra      (bb/allocate (* 4 width height))
        set-color (fn [i r g b a]
                    (bb/position bgra (* 4 i))
                    (bb/put-unsigned-byte bgra b)
                    (bb/put-unsigned-byte bgra g)
                    (bb/put-unsigned-byte bgra r)
                    (bb/put-unsigned-byte bgra a))]
    (cond 
      ;; palette
      (= size (* width height))
      (let [palette (.slice buf (int (+ 12 (* width height))) (int (* 3 256)))]
        (bb/position buf 12)
        (dotimes [i (* width height)]
          (let [color     (bb/get-unsigned-byte buf)
                [r g b a] (palette-color palette color)]
            (set-color i r g b a))))

      ;; rgb
      (= size (* 3 width height))
      (do
        (bb/position buf 12)
        (dotimes [i (* width height)]
          (let [b (bb/get-unsigned-byte buf)
                g (bb/get-unsigned-byte buf)
                r (bb/get-unsigned-byte buf)]
            (set-color i r g b 255))))

      :else
      (throw (Exception. (str "Unexpeceted PCX dimensions width=" width ", height=" height ", size=" size))))
    (image/from-bgra bgra width height)))

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
;; Frame pixel data (format 2)
;;   offset:+32 length:(2 * H * W / 32) :: shorts :: fragment relative offsets (Rs)
;; Repeat until total length of 32 is accumulated
;;   offset:+(32 + R)     length:1 :: byte :: segment (S) == 3 bits code (C) + 5 bits length - 1 (L)
;; if C == 7
;;   offset:+(32 + R + 1) length:(L + 1) :: bytes :: color indices from palette
;; else
;;   offset:+(32 + R + 1) length:0 :: nothing :: repeat color C times (L + 1)
(defn read-def-frame [^ByteBuffer buf set-color frame-offset]
  (bb/position buf frame-offset)
  (let [_size       (bb/get-int buf)
        format      (bb/get-int buf)
        full-width  (bb/get-int buf)
        full-height (bb/get-int buf)
        width       (bb/get-int buf)
        height      (bb/get-int buf)
        left        (bb/get-int buf)
        top         (bb/get-int buf)]
    ; (prn format full-width full-height width height left-margin top-margin)
    (case format
      0
      (doseq [y (range 0 height)
              x (range 0 width)]
        (set-color x y (.get buf)))

      1
      (doseq [[y rel-offset] (core/forv [y (range 0 height)]
                               [y (bb/get-int buf)])]
        (bb/position buf (+ frame-offset 32 rel-offset))
        (loop [total-length 0]
          (let [code   (bb/get-unsigned-byte buf)
                length (long (inc (bb/get-unsigned-byte buf)))]
            (if (= 255 code)
              (dotimes [dx length]
                (set-color (+ total-length dx) y (bb/get-unsigned-byte buf)))
              (dotimes [dx length]
                (set-color (+ total-length dx) y code)))
            (when (< (+ total-length length) width)
              (recur (+ total-length length))))))
      
      2
      (doseq [[y rel-offset] (core/forv [y (range 0 height)]
                               [y (bb/get-unsigned-short buf)])]
        (bb/position buf (+ frame-offset 32 rel-offset))
        (loop [total-length 0]
          (let [segment (bb/get-unsigned-byte buf)
                code    (bit-shift-right segment 5)
                length  (inc (bit-and segment 0x1F))]
            (if (= 7 code)
              (dotimes [dx length]
                (set-color (+ total-length dx) y (bb/get-unsigned-byte buf)))
              (dotimes [dx length]
                (set-color (+ total-length dx) y code)))
            (when (< (+ total-length length) width)
              (recur (+ total-length length))))))
      
      3
      (doseq [[x y rel-offset] (core/forv [y (range 0 height)
                                           x (range 0 width 32)]
                                 [x y (bb/get-unsigned-short buf)])]
        (bb/position buf (+ frame-offset 32 rel-offset))
        (loop [total-length 0]
          (let [segment (bb/get-unsigned-byte buf)
                code    (bit-shift-right segment 5)
                length  (inc (bit-and segment 0x1F))]
            (if (= 7 code)
              (dotimes [dx length]
                (set-color (+ x total-length dx) y (bb/get-unsigned-byte buf)))
              (dotimes [dx length]
                (set-color (+ x total-length dx) y code)))
            (when (< (+ total-length length) 32)
              (recur (+ total-length length)))))))))

; (defn read-def [^ByteBuffer buf]
;   (let [type         (bb/get-int buf 0)
;         blocks-len   (bb/get-int buf 12)]
;     (if (> blocks-len 1000)
;       (println "    Failed to parse, blocks:" blocks-len "type:" type)
;       (let [_            (bb/position buf (+ 16 (* 3 256)))
;             blocks       (core/forv [_ (range blocks-len)]
;                            (let [id         (bb/get-int buf)
;                                  frames-len (bb/get-int buf)]
;                              (bb/advance buf 8) ;; unknown
;                              {:id         id
;                               :frames-len frames-len
;                               :names      (core/repeatedlyv frames-len #(bb/get-string buf 13))
;                               :offsets    (core/repeatedlyv frames-len #(bb/get-int buf))}))
;             frames       (core/forv [block  blocks
;                                      offset (:offsets block)]
;                            (bb/position buf (+ offset 16))
;                            (let [width  (bb/get-int buf)
;                                  height (bb/get-int buf)
;                                  left   (bb/get-int buf)
;                                  top    (bb/get-int buf)]
;                              {:width  width
;                               :height height
;                               :left   left
;                               :top    top
;                               :right  (+ left width)
;                               :bottom (+ top height)}))
;             frame-left   (transduce (map :left) min Long/MAX_VALUE frames)
;             frame-top    (transduce (map :top) min Long/MAX_VALUE frames)
;             frame-width  (- (transduce (map :right) max 0 frames) frame-left)
;             frame-height (- (transduce (map :bottom) max 0 frames) frame-top)
;             max-frames   (transduce (map :frames-len) max 0 blocks)
;             bgra         (bb/allocate (* 4 max-frames frame-width blocks-len frame-height))]
;         (doseq [[y block]       (core/zip (range) blocks)
;                 [x name offset] (core/zip (range) (:names block) (:offsets block))
;                 :let 
          
        

;; LOD file format
;;   offset:8  length:4        :: int :: Number of files (N)
;;   offset:92 length:(32 * N) :: Info * N :: Info about files
;; 
;; Info
;;   offset:+0  length:16      :: string :: File name
;;   offset:+16 length:4       :: int    :: File content offset
;;   offset:+20 length:4       :: int    :: File original size
;;   offset:+24 length:4       :: int    :: ???
;;   offset:+28 length:4       :: int    :: File compressed size
;; 
;; File (uncompressed, csize == 0)
;;   offset:<offset> length:<size> :: bytes :: File content
;;
;; File (ZIP compressed, csize > 0, csize != size)
;;   offset:<offset> length:<csize> :: bytes :: File content

(defn extract-file [^ByteBuffer buf file]
  (let [{:keys [^String name offset size csize]} file
        offset (int offset)
        size   (int size)
        csize  (int csize)]
    #_(println "    offset:" offset "size:" size "csize:" csize)
    (if (> csize 0)
      (let [res      ^ByteBuffer (bb/allocate size)
            inflater (Inflater.)]
        (.setInput inflater (.slice buf offset csize))
        (.inflate inflater res)
        (.end inflater)
        res)
      (.slice buf offset size))))

(defn files [^ByteBuffer buf]
  (let [total (bb/get-int buf 8)]
    (core/forv [i (range total)]
      (bb/position buf (+ 92 (* 32 i)))
      (let [name   (bb/get-string buf 16)
            offset (bb/get-int buf)
            size   (bb/get-int buf)
            _      (bb/get-int buf)
            csize  (bb/get-int buf)]
        {:name   name
         :offset offset
         :size   size
         :csize  csize}))))
    
(defn extract
  ([from] (extract from -1))
  ([from amount]
   (assert (str/ends-with? from ".lod"))
   (println "Extracting" from)
   (bb/with-mapped [buf from]
     (let [t0    (System/currentTimeMillis)
           dir   (subs from 0 (- (count from) 4))
           _     (Files/createDirectories (core/path dir) (make-array FileAttribute 0))
           files (cond->> (files buf)
                   (pos? amount) (take amount))
           res   (core/forv [file files
                             :let [name  (:name file)
                                   _     (println " " name)
                                   bytes (extract-file buf file)]
                             [name' bytes'] (cond
                                              (re-matches #"(?i).+\.pcx" name)
                                              [[(str/replace name #"(?i)\.pcx$" ".png") (-> bytes read-pcx image/to-png)]]
                                              
                                              :else
                                              [[name bytes]])]
                   (bb/write bytes' (str dir "/" name'))
                   #_(when (re-matches #"(?i).+\.def" name)
                       (read-def bytes))
                   name')]
       (println "DONE" (count res) "files in" (- (System/currentTimeMillis) t0) "ms")))))

(defn -main [& args]
  (extract "resources/Data/H3ab_bmp.lod")
  (extract "resources/Data/H3ab_spr.lod")
  (extract "resources/Data/H3bitmap.lod")
  (extract "resources/Data/H3sprite.lod"))

(comment
  (extract "resources/Data/H3ab_bmp.lod" 32)
  
  (require '[clj-async-profiler.core :as prof])
  (prof/profile
    (extract "resources/Data/H3ab_bmp.lod"))
  (prof/serve-files 8080))
