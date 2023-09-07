(ns ^{:clojure.tools.namespace.repl/load false}
  homm.viewer.state
  (:require
    [io.github.humbleui.cursor :as cursor]))

(def *window
  (atom nil))

(def *app
  (atom nil))

(def *state
  (atom
    {:lod nil
     :file nil
     :filter {:text ""}}))

(def *filter
  (cursor/cursor *state :filter))
