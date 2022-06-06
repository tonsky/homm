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
      (let [palette (.slice buf (+ 12 (* width height)) (* 3 256))]
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
