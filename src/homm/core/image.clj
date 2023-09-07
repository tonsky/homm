(ns homm.core.image
  (:require
    [homm.core.byte-buffer :as bb])
  (:import
    [io.github.humbleui.skija ColorAlphaType EncoderPNG Image ImageInfo]
    [java.nio ByteBuffer]))

(set! *warn-on-reflection* true)

(defn from-bgra ^Image [^ByteBuffer buf ^long width ^long height]
  (Image/makeRaster (ImageInfo/makeS32 width height ColorAlphaType/PREMUL) ^bytes (bb/array buf) (* width 4)))

(defn ^ByteBuffer to-png [^Image image]
  (with-open [data (EncoderPNG/encode image)]
    (bb/wrap (.getBytes data))))
