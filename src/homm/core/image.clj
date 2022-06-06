(ns homm.core.image
  (:require
    [homm.core.byte-buffer :as bb])
  (:import
    [io.github.humbleui.skija ColorAlphaType EncodedImageFormat Image ImageInfo]
    [java.nio ByteBuffer]))

(set! *warn-on-reflection* true)

(defn from-bgra [^ByteBuffer buf ^long width ^long height]
  (Image/makeRaster (ImageInfo/makeS32 width height ColorAlphaType/PREMUL) ^bytes (bb/array buf) (* width 4)))

(defn ^ByteBuffer to-png [^Image image]
  (with-open [data (.encodeToData image EncodedImageFormat/PNG)]
    (bb/wrap (.getBytes data))))
