(ns homm.viewer
  (:require
    [clojure.core.server :as server]
    [clojure.string :as str]
    [homm.core :as core]
    [homm.core.byte-buffer :as bb]
    [homm.extract :as extract]
    [homm.extract.def :as extract.def]
    [homm.viewer.state :as state]
    [io.github.humbleui.app :as app]
    [io.github.humbleui.debug :as debug]
    [io.github.humbleui.paint :as paint]
    [io.github.humbleui.protocols :as protocols]
    [io.github.humbleui.window :as window]
    [io.github.humbleui.ui :as ui]
    [io.github.humbleui.ui.image :as ui.image])
  (:import
    [java.util List]
    [java.util.function Predicate]
    [java.nio ByteBuffer]
    [java.nio.file Files Path]
    [io.github.humbleui.skija Image]))

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

(defn dispatch-show-file [lod-buf file]
  (second (re-matches #".*\.([^.]+)" (str/lower-case (:name file)))))

(defmulti show-file
  dispatch-show-file)

#_(defmethod show-file "txt" [lod-buf file]
    (let [bytes (.array ^ByteBuffer (extract/extract-file lod-buf file))
          lines (str/split-lines (String. ^bytes bytes "UTF-8"))]
      (ui/vscrollbar
        (ui/padding 10 20
          (ui/column
            (interpose (ui/gap 0 10)
              (for [line lines]
                (ui/paragraph line))))))))

(defmethod show-file "pcx" [lod-buf file]
  (let [^Image image (extract/read-pcx (extract/extract-file lod-buf file))
        width        (.getWidth image)
        height       (.getHeight image)
        aspect       (/ (.getWidth image) (.getHeight image))
        comp         (ui.image/map->AnImage {:image image})]
    (ui/with-bounds ::bounds
      (ui/dynamic ctx [bounds (::bounds ctx)]
        (let [aspect' (/ (:width bounds) (:height bounds))
              w'      (min (* 2 width) (:width bounds))
              h'      (/ w' aspect)
              h''     (min (* 2 height) (:height bounds))
              w''     (* h'' aspect)]
          (ui/center
            (ui/width (min w' w'')
              (ui/height (min h' h'')
                comp))))))))

(defmethod show-file "def" [lod-buf file]
  (let [info         (extract.def/extract-info (extract/extract-file lod-buf file))
        ^Image image (:image info)
        width        (.getWidth image)
        height       (.getHeight image)
        aspect       (/ (.getWidth image) (.getHeight image))
        comp         (ui.image/map->AnImage {:image image})]
    (ui/with-bounds ::bounds
      (ui/dynamic ctx [bounds (::bounds ctx)]
        (let [aspect' (/ (:width bounds) (:height bounds))
              w'      (min (* 2 width) (:width bounds))
              h'      (/ w' aspect)
              h''     (min (* 2 height) (:height bounds))
              w''     (* h'' aspect)]
          (ui/center
            (ui/width (min w' w'')
              (ui/height (min h' h'')
                comp))))))))

(defmethod show-file :default [lod-buf file]
  (ui/center
    (ui/label (:name file))))

(defn prev-file [state]
  (let [{:keys [files file]} state
        index (.indexOf ^List files file)]
    (assoc state :file (if (> index 0) (nth files (dec index)) nil))))

(defn next-file [state]
  (let [{:keys [files file]} state
        index (.indexOf ^List files file)]
    (assoc state :file (if (< index (dec (count files))) (nth files (inc index)) nil))))

(defn view-file [{:keys [lod-buf file]}]
  (ui/key-listener
    {:on-key-down
     (fn [e]
       (case (:key e)
         :escape (swap! state/*state assoc :file nil)
         :left   (swap! state/*state prev-file)
         :right  (swap! state/*state next-file)
         nil))}
    (ui/padding 10
      (ui/column
        [:stretch 1
         (ui/halign 0.5
           (show-file lod-buf file))]
        (ui/gap 0 10)
        (ui/halign 0.5
          (ui/label (:name file)))))))

(defn comp-grid [w h per-row per-page lod-buf page files]
  (ui/grid 
    (for [row (->> files
                (drop (* page per-page))
                (take per-page)
                (partition-all per-row))]
      (for [file row]
        (ui/width w
          (ui/height h
            (ui/clickable
              {:on-click
               (fn [_]
                 (swap! state/*state assoc :file file))}
              (ui/padding 1
                (show-file lod-buf file)))))))))

(defn comp-pages [page pages]
  (ui/key-listener
    {:on-key-down
     (fn [e]
       (case (:key e)
         :left  (swap! state/*state update :page #(-> % (- 1) (max 0) (min (dec pages))))
         :right (swap! state/*state update :page #(-> % (+ 1) (max 0) (min (dec pages))))
         nil))}
    (ui/padding 0 5
      (ui/halign 0.5
        (ui/row
          (for [i (range 0 pages)]
            (ui/clickable
              {:on-click
               (fn [_]
                 (swap! state/*state assoc
                   :page i))}
              (let [label (ui/padding 5 5
                            (ui/label (str (inc i))))]
                (ui/dynamic ctx [{:hui/keys [hovered?]} ctx
                                 selected? (= i page)]
                  (cond
                    selected? (ui/rect (paint/fill 0xFFCCCCCC) label)
                    hovered?  (ui/rect (paint/fill 0xFFEEEEEE) label)
                    :else     label))))))))))

(defn view-grid [{:keys [page files lod-buf]}]
  (ui/with-bounds ::bounds
    (ui/dynamic ctx [{:keys [width height]} (::bounds ctx)]
      (let [per-row  (quot width 100)
            w        (/ width per-row)
            height   (- height 30)
            per-col  (quot height 100)
            h        (/ height per-col)
            per-page (* per-row per-col)
            pages    (-> (count files) (- 1) (quot per-page) (+ 1))
            page     (-> page (min (dec pages)) (max 0))]
        (ui/column
          [:stretch 1
           (comp-grid w h per-row per-page lod-buf page files)]
          (comp-pages page pages))))))

(def app
  (ui/default-theme {}
    (ui/row
      (ui/width 150
        (ui/column
          (ui/padding 0 10
            (vlist (lods) :lod {:label-fn #(.getFileName ^Path %)
                                :on-select (fn [lod]
                                             (let [lod-buf (bb/mmap (str lod))
                                                   files   (->> (extract/files lod-buf)
                                                             (remove #(str/ends-with? (:name %) ".msk"))
                                                             vec)]
                                               (swap! state/*state assoc
                                                 :lod-buf lod-buf
                                                 :files   files
                                                 :page    0
                                                 :file    nil)))}))
          #_(ui/rect (paint/fill 0xFFDDDDDD) (ui/gap 0 2))
          #_(ui/padding 10 10
              (ui/text-field state/*filter))
          #_[:stretch 1
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
       (ui/dynamic _ [{:keys [lod file page]} @state/*state]
         (cond
           (and lod file)
           (view-file @state/*state)
        
           lod
           (view-grid @state/*state)
           
           :else
           (ui/gap 0 0)))])))

(reset! state/*app app)

(defn start! []
  (ui/start-app!
    (let [screen (app/primary-screen)]
      (reset! state/*window 
        (ui/window
          {:title    "HoMM III Resource Viewer"
           :mac-icon "src/homm/viewer/icon.icns"
           :screen   (:id screen)
           :width    600
           :height   600
           :x        :center
           :y        :center}
          state/*app))))
  (reset! protocols/*debug? false))

(defn -main [& args]
  (start!)
  (let [{port "--port"
         :or {port "5555"}} (apply array-map args)
        port (parse-long port)]
    (server/start-server
      {:name          "repl"
       :port          port
       :accept        'clojure.core.server/repl
       :server-daemon false})
    (println "Started Server Socket REPL on port" port)))
