(ns homm.viewer
  (:require
    [clojure.core.server :as server]
    [clojure.string :as str]
    [homm.core :as core]
    [homm.core.byte-buffer :as bb]
    [homm.extract :as extract]
    [homm.viewer.state :as state]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.debug :as debug]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.window :as window]
    [io.github.humbleui.ui :as ui]
    [io.github.humbleui.ui.image :as ui.image])
  (:import
    [java.util.function Predicate]
    [java.nio ByteBuffer]
    [java.nio.file Files Path]))

(defn lods []
  (-> (core/path "resources" "Data")
    (Files/list)
    (.filter (reify Predicate
               (test [_ path]
                 (-> ^Path path .getFileName str (str/ends-with? ".lod")))))
    (.toList)))

(defn vlist [xs key {:keys [key-fn label-fn on-select]
                     :or {key-fn    identity
                          label-fn  identity
                          on-select (fn [_])}}]
  (ui/column
    (for [x xs
          :let [xk (key-fn x)
                xl (label-fn x)]]
      (ui/clickable
        {:on-click (fn [_]
                     (swap! state/*state assoc key xk)
                     (on-select xk))}
        (let [label (ui/padding 10 10
                      (ui/label xl))]
          (ui/dynamic ctx [selected? (= xk (key @state/*state))
                           hovered?  (:hui/hovered? ctx)]
            (cond
              selected? (ui/rect (paint/fill 0xFFB3D7FF) label)
              hovered?  (ui/rect (paint/fill 0xFFEEEEEE) label)
              :else     label)))))))

(defmulti show-file (fn [lod file]
                      (second (re-matches #".*\.([^.]+)" (str/lower-case (:name file))))))

(defmethod show-file "txt" [lod file]
  (let [bytes (bb/with-mapped [buf (str lod)]
                (.array ^ByteBuffer (extract/extract-file buf file)))
        lines (str/split-lines (String. ^bytes bytes "UTF-8"))]
    
    (ui/vscrollbar
      (ui/padding 10 20
        (ui/column
          (interpose (ui/gap 0 10)
            (for [line lines]
              (ui/paragraph line))))))))

(defmethod show-file "pcx" [lod file]
  (let [image  (bb/with-mapped [buf (str lod)]
                 (extract/read-pcx (extract/extract-file buf file)))
        width  (.getWidth image)
        height (.getHeight image)]
    (ui/center
      (ui/with-bounds ::bounds
        (ui/dynamic ctx [bounds (::bounds ctx)]
          (let [scale (min
                        (/ (:width bounds) width)
                        (/ (:height bounds) height)
                        2)]
            (ui/width (* scale width)
              (ui/height (* scale height)
                (ui.image/map->AnImage {:image image})))))))))

(defmethod show-file :default [lod file]
  (ui/center
    (ui/label (:name file))))

(def app
  (ui/default-theme {}
    (ui/row
      (ui/width 150
        (ui/column
          (ui/padding 0 10
            (vlist (lods) :lod {:label-fn #(.getFileName ^Path %)
                                :on-select (fn [_]
                                             (swap! state/*state assoc :file nil))}))
          (ui/rect (paint/fill 0xFFDDDDDD) (ui/gap 0 2))
          (ui/padding 10 10
            (ui/text-field state/*filter))
          [:stretch 1
           (ui/dynamic _ [lod (:lod @state/*state)]
             (if lod
               (let [files (bb/with-mapped [buf (str lod)]
                             (extract/files buf))]
                 (ui/vscrollbar
                   (ui/dynamic _ [substring (:text @state/*filter)]
                     (let [files' (filterv #(str/includes? (str/lower-case (:name %)) (str/lower-case substring)) files)]
                       (vlist files' :file {:label-fn :name})))))
               (ui/gap 0 0)))]))
      (ui/rect (paint/fill 0xFFDDDDDD) (ui/gap 2 0))
      [:stretch 1
       (ui/dynamic _ [lod  (:lod @state/*state)
                      file (:file @state/*state)]
         (if (and lod file)
           (show-file lod file)
           (ui/gap 0 0)))])))

(reset! state/*app app)

(defn -main [& args]
  (ui/start-app!
    (let [screen (app/primary-screen)]
      (reset! state/*window 
        (ui/window
          {:title    "HoMM III Resource Viewer"
           :mac-icon "dev/homm/viewer/icon.icns"
           :screen   (:id screen)
           :width    600
           :height   600
           :x        :center
           :y        :center}
          state/*app))))
  (let [{port "--port"
         :or {port "5555"}} (apply array-map args)
        port (parse-long port)]
    (println "Started Server Socket REPL on port" port)
    (server/start-server
      {:name          "repl"
       :port          port
       :accept        'clojure.core.server/repl
       :server-daemon false})))
