(ns ^:figwheel-always tumblr-reframe.core
  (:require-macros [reagent.ratom :refer [reaction]]
                   [secretary.core :refer [defroute]])
  (:require
   [reagent.core :as reagent :refer [atom]]
   [re-frame.core :refer [register-handler
                          path
                          register-sub
                          dispatch
                          dispatch-sync
                          subscribe]]
   [secretary.core :as secretary]
   [goog.events :as events]
   [goog.history.EventType :as EventType])
  (:import [goog.net Jsonp]
           [goog Uri]
           [goog History]))

(enable-console-print!)

;;; state
;;; ------------------------------------------------------------------------------

(def initial-state
  {:mode         :search
   :search-term  ""
   :search-type  :tag
   :current-input ""
   :grid-type    :horizontal
   :entries      []
   :before       0
   :page         0
   :window-width (.. js/window -innerWidth)
   :error-string ""})

(def base-row-height 500)
(def base-col-width 800)

(defn perform-tag-search [search-term before]
  ;; goog.Jsonp.send [uri] [query params] [resp handler] [error handler]
  (.send (Jsonp. (Uri. "http://api.tumblr.com/v2/tagged"))
         ;; query parameters ---------
         (clj->js
          {:tag     search-term
           :before  before
           :api_key "pekKZHs4hKvshK1NRyXlawVhO203uYg0MMfGj5Tq8ts6M1Wq9Z"})
         ;; response handler ---------
         (fn [v]
           (case (.. v -meta -status)
             200
             ;; resp ok handler ------
             (dispatch
              [:search-result
               (filter (fn [item] (= (:type item) "photo"))
                       (js->clj (.. v -response) :keywordize-keys true))])
             ;; resp error handler ---
             (dispatch [:error (str "API Error: " (js->clj (.. v -meta -msg) :kewordize-keys true))])))
         ;; req error handler --------
         #(dispatch
           [:error (str "Network Error")])))

(defn perform-user-search [user-name page]
  ;; goog.Jsonp.send [uri] [query params] [resp handler] [error handler]
  (.send (Jsonp. (Uri. (str "http://api.tumblr.com/v2/blog/" user-name ".tumblr.com/posts/photo")))
         ;; query parameters ---------
         (clj->js
          {:offset  (* page 20)
           :api_key "pekKZHs4hKvshK1NRyXlawVhO203uYg0MMfGj5Tq8ts6M1Wq9Z"})
         ;; response handler ---------
         (fn [v]
           (case (.. v -meta -status)
             200
             ;; resp ok handler ------
             (let [posts (:posts (js->clj (.. v -response) :keywordize-keys true))]
               (dispatch
                [:search-result posts]))
             ;; resp error handler ---
             (dispatch [:error (str "API Error: " (js->clj (.. v -meta -msg) :kewordize-keys true))])))
         ;; req error handler --------
         #(dispatch
           [:error (str "Network Error")])))

;;; routing
;;; ------------------------------------------------------------------------------

(defonce h (History.))

(defroute reset "/" []
  (dispatch [:initialize]))

(defroute search-tag "/tag/:tag" [tag]
  (dispatch [:update-input (js/decodeURIComponent tag)])
  (dispatch [:new-tag-search]))

(defroute search-user "/user/:user-name" [user-name]
  (dispatch [:update-input (js/decodeURIComponent user-name)])
  (dispatch [:new-user-search]))

;;; handlers
;;; ------------------------------------------------------------------------------

(register-handler
 :initialize
 (fn [db _]
   (merge db initial-state)))

(register-handler
 :error
 (fn [db [_ error-string]]
   (assoc db :mode        :error
          :error-string error-string)))

(register-handler
 :update-input
 (fn [db [_ current-input]]
   (assoc db :current-input current-input)))

(register-handler
 :new-tag-search
 (fn [db [_]]
   (.blur (.getElementById js/document "input-box"))
   (perform-tag-search (:current-input db) 0)
   (assoc db :search-term (:current-input db)
          :search-type :tag
          :mode        :loading
          :entries     []
          :page        0
          :before      0)))

(register-handler
 :new-user-search
 (fn [db [_]]
   (.blur (.getElementById js/document "input-box"))
   (perform-user-search (:current-input db) 0)
   (assoc db :search-term (:current-input db)
          :search-type :user
          :mode        :loading
          :entries     []
          :page        0
          :before      0)))

(register-handler
 :continue-search
 (fn [db [_]]
   (if (= :tag (:search-type db))
     (perform-tag-search (:search-term db) (:before db))
     (perform-user-search (:search-term db) (:page db))
     )
   (assoc db :mode :loading)))

(defn normalize-entry [entry]
  (let
      [timestamp (:timestamp entry)
       post-url  (:post_url entry)
       photos    (-> entry :photos)
       title     (clojure.string/replace (:slug entry)  #"-"  " ")
       ]
    (for [photo photos]
      (let [{img-h   :height
             img-w   :width
             img-url :url} (-> photo :alt_sizes second)
            adj-w (Math/round (* img-w (/ base-row-height img-h)))]
        {:w adj-w
         :h base-row-height
         :orig-w img-w
         :orig-h img-h
         :adj-w adj-w
         :url img-url
         :post-url post-url
         :title title
         :timestamp timestamp}))))

(defn adjust-row [row adjust-ratio]
  (map (fn [img]
         (-> img
             (assoc :w (Math/ceil (* (:adj-w img) adjust-ratio)))
             (assoc :h (Math/ceil (* base-row-height adjust-ratio)))
             (update-in [:x] #(Math/ceil (* % adjust-ratio)))
             ))
       row))

(defn build-v-grid [current-entrys window-width]
  (let [col-n (inc (Math/floor (/ window-width base-col-width)))
        col-w (/ window-width col-n)]
    (loop [entrys   current-entrys
           coll    []
           idx     0
           offsets (zipmap (range col-n) (repeat 0))]
      (if (empty? entrys) coll
          (let [entry  (first entrys)]
            (if (or (nil? (:orig-w entry)) (nil? (:orig-h entry)))
              ;; nil size found - drop the image
              (recur (rest entrys) coll idx offsets)
              ;; image ok - place image in grid
              (let [offset-n    (mod idx col-n)
                    offset      (apply min-key second offsets)
                    offset-x    (* col-w (first offset))
                    offset-y    (second offset)
                    new-height  (int (* (:orig-h entry) (/ col-w (:orig-w entry))))
                    new-offsets (update-in offsets [(first offset)] + new-height)
                    new-entry (assoc entry
                                     :x offset-x
                                     :y offset-y
                                     :w col-w
                                     :h new-height)]
                (recur
                 (rest entrys)
                 (conj coll new-entry)
                 (inc idx)
                 new-offsets))))))))

(defn build-h-grid [current-entries window-width]
  (loop [entries  current-entries
         row      []
         row-w    0
         v-offset 0
         acc      []]
    (if (empty? entries) (into acc row)
        (let [{img-w :adj-w :as entry} (first entries)]
          (if (nil? img-w)
            ;; nil size found - drop the image
            (recur (rest entries) row row-w v-offset acc)
            ;; image ok - place image in grid
            (let [new-entry (assoc entry :x row-w :y v-offset)
                  new-row  (conj row new-entry)
                  new-row-w (+ row-w img-w)]
              (if (>= new-row-w window-width)
                ;; went over the width of the window
                ;; - new row
                ;; - adjust size of images to fit justified
                (let [adjust-ratio (/ window-width new-row-w)
                      new-v-offset (Math/round (+ v-offset (* adjust-ratio base-row-height)))
                      adjusted-row (adjust-row new-row adjust-ratio)
                      new-acc      (into acc adjusted-row)]
                  (recur (rest entries) [] 0 new-v-offset new-acc))
                (recur (rest entries) new-row new-row-w v-offset acc))))))))

(register-handler
 :search-result
 (fn [db [_ new-entries]]
   (if (not-empty new-entries)
     (let [sorted-new-entries (->> new-entries
                                   (sort-by :timestamp #(compare %2 %1))
                                   (filter #(<= 2 (count (-> % :photos first :alt_sizes))))
                                   (map normalize-entry)
                                   (flatten))
           sorted-entries (into (:entries db) sorted-new-entries)
           new-entries (case (:grid-type db)
                         :vertical (build-v-grid sorted-entries (:window-width db))
                         (build-h-grid sorted-entries (:window-width db)))]
       (-> db
           (assoc :entries new-entries)
           (assoc :before (-> sorted-entries last :timestamp))
           (update-in [:page] inc)
           (assoc :mode :loaded)
           ))
     (-> db (assoc :mode :finished)))))

(register-handler
 :resize
 (fn [db [_]]
   (let [new-width (.. js/window -innerWidth)
         new-entries (case (:grid-type db)
                       :vertical (build-v-grid (:entries db) new-width)
                       (build-h-grid (:entries db) new-width))]
     (assoc db :window-width new-width
            :entries new-entries
            ))))

(register-handler
 :scroll
 (fn [db [_]]
   (when
       (and (= :loaded (:mode db))
            (> 100 (-  (.. js/document -documentElement -scrollHeight)
                       (.. js/window -scrollY)
                       (.. js/window -innerHeight))))
     (dispatch [:continue-search]))
   db))

(register-handler
 :switch-grid
 (fn [db [_]]
   (let [current-grid-type (:grid-type db)
         new-grid-type (if (= :horizontal current-grid-type)
                         :vertical
                         :horizontal)
         new-entries (case new-grid-type
                       :vertical (build-v-grid (:entries db) (:window-width db))
                       (build-h-grid (:entries db) (:window-width db)))
         ]
     (assoc db :entries new-entries
            :grid-type new-grid-type))))

;;; debug
;;; ------------------------------------------------------------------------------

(register-handler
 :dump
 (fn [db _]
   (println db)
   db))

;;; subscriptions
;;; ------------------------------------------------------------------------------

(register-sub
 :mode
 (fn
   [db _]
   (reaction (:mode @db))))

(register-sub
 :search-term
 (fn
   [db _]
   (reaction (:search-term @db))))

(register-sub
 :current-input
 (fn
   [db _]
   (reaction (:current-input @db))))

(register-sub
 :entries
 (fn
   [db _]
   (reaction (:entries @db))))

(register-sub
 :window-width
 (fn
   [db _]
   (reaction (:window-width @db))))

(register-sub
 :error-text
 (fn
   [db _]
   (reaction (:error-text @db))))

(register-sub
 :search-type
 (fn
   [db _]
   (reaction (:search-type @db))))

;;; views
;;; ------------------------------------------------------------------------------

(defn input-form []
  (let [current-search (subscribe [:search-term])
        current-input  (subscribe [:current-input])
        maybe-new-tag-search #(when (not-empty @current-input)
                                (.setToken h (str "/tag/" (js/encodeURIComponent @current-input))))
        maybe-new-user-search #(when (not-empty @current-input)
                                 (.setToken h (str "/user/" (js/encodeURIComponent @current-input))))]
    (fn []
      [:form
       ;; text-input
       [:input {:type "text"
                :id   "input-box"
                :default-value @current-search
                :value @current-input
                :on-change #(dispatch-sync [:update-input (.. % -target -value)])
                :on-key-down #(when (= 13 (.. % -keyCode))
                                (do
                                  (.preventDefault %)
                                  (if (.. % -shiftKey)
                                    (maybe-new-user-search)
                                    (maybe-new-tag-search))))
                }]
       ;; submit-button - tag
       [:input {:type "button"
                :value "Tag Search"
                :on-click maybe-new-tag-search}]
       ;; submit-button - user
       [:input {:type "button"
                :value "User Search"
                :on-click maybe-new-user-search}]
       ])))

(defn grid-entry [{:keys [x y w h url post-url title]}]
  [:li {:style {:left     (str x "px")
                :top      (str y "px")
                :width    (str w "px")}}
   [:a {:href post-url :target "_blank"}
    [:img {:src url
           :alt title
           :style {:padding "3px" :display "block"}
           :width (str (- w 6) "px")
           :height (str (- h 6) "px")}]]])

(defn entry-list []
  (let [entries (subscribe [:entries])]
    (when (not-empty @entries)
      (into [:ul {:id "gridlist"}]
            (mapv grid-entry @entries)))))

(defn header []
  (let [search-term (subscribe [:search-term])
        mode        (subscribe [:mode])
        entries     (subscribe [:entries])
        search-type (subscribe [:search-type])
        error-text  (subscribe [:error-text])
        ]
    [:div {:id "header"}
     (if (empty? @search-term)
       [:h1 "Enter Search:"]
       [:h1 (if (= :user @search-type) "User Search: " "Tag Search: ") @search-term])
     [input-form]
     (case @mode
       :loading  [:h2 "loading"]
       :finished [:h2 (if (empty? @entries)
                        "No entries found."
                        "Found All Entries.")]
       :error    [:h2 (:error-text error-text)]
       "")]))

(defn application []
  (fn [] [:div [header] [entry-list]]))

;;; run
;;; ------------------------------------------------------------------------------

(defonce intialize
  (do
    (.addEventListener
     js/window "resize" #(dispatch [:resize]))
    (.addEventListener
     js/window "scroll" #(dispatch [:scroll]))
    (.addEventListener js/window "keydown"
                       #(case (.. % -keyCode)
                          27 (.setToken h (str "/"))
                          71 (when (not= "INPUT" (.. % -target -nodeName))
                               (dispatch [:switch-grid]))
                          :noop
                          ))
    (dispatch-sync [:initialize])
    (secretary/set-config! :prefix "#")
    (goog.events/listen h EventType/NAVIGATE #(secretary/dispatch! (.-token %)))
    (doto h (.setEnabled true))
    ))

(defn ^:export run []
  (reagent/render [application]
                  (js/document.getElementById "app")))
