(ns ^:figwheel-always tumblr-reframe.core
  (:require-macros [reagent.ratom :refer [reaction]])   
  (:require
    [reagent.core :as reagent :refer [atom]]
    [re-frame.core :refer [register-handler
                           path
                           register-sub
                           dispatch
                           dispatch-sync
                           subscribe]])
  (:import [goog.net Jsonp]
           [goog Uri]))

(enable-console-print!)

; state
; ------------------------------------------------------------------------------

(def initial-state
  {:mode         :search
   :search-term  ""
   :entries      []
   :before       0
   :page         0
   :window-width (.. js/window -innerWidth)
   :error-string ""
   })

(def base-row-height 340)

(defn perform-search [search-term before]
  ; goog.Jsonp.send [uri] [query params] [resp handler] [error handler]
  (.send (Jsonp. (Uri. "http://api.tumblr.com/v2/tagged")) 
         ; query parameters ---------
         (clj->js 
           {:tag     search-term
            :before  before
            :api_key "pekKZHs4hKvshK1NRyXlawVhO203uYg0MMfGj5Tq8ts6M1Wq9Z"})
         ; response handler ---------
         (fn [v]
           (case (.. v -meta -status)
             200 
             ; resp ok handler ------
             (dispatch
               [:search-result 
                (filter (fn [item] (= (:type item) "photo"))
                        (js->clj (.. v -response) :keywordize-keys true))])
             ; resp error handler ---
             (dispatch [:error (str "API Error: " (js->clj (.. v -meta -msg) :kewordize-keys true))])))
         ; req error handler --------
         #(dispatch 
            [:error (str "Network Error")])
         ))

; handlers
; ------------------------------------------------------------------------------

(register-handler 
  :initialize
  (fn [db _]
    (merge db initial-state)))

(register-handler 
  :error
  (fn [db [_ error-string]]
    (assoc db :mode        :error
              :error-string error-string
              )))

(register-handler 
  :new-search
  (fn [db [_ search-term]]
    (perform-search search-term 0)
    (assoc db :search-term search-term
              :mode        :loading
              :entries     []
              :page        0
              :before      0
              )))

(register-handler :dump
  (fn [db _]
    (println db)
    db
    ))

(register-handler 
  :continue-search
  (fn [db [_]]
    (perform-search (:search-term db) (:before db))
    (assoc db :mode :loading)))

(defn normalize-entry [entry]
  (let 
    [timestamp (:timestamp entry)
     {img-h   :height
      img-w   :width
      img-url :url
      } (-> entry :photos first :alt_sizes second)
      adj-w (Math/round (* img-w (/ base-row-height img-h)))
      title (clojure.string/replace (:slug entry)  #"-"  " ")
      ]
    {:w adj-w
     :h base-row-height
     :orig-w adj-w
     :orig-h base-row-height
     :url img-url
     :title title
     :timestamp timestamp
     }))

(defn adjust-row [row adjust-ratio]
  (map (fn [img] 
         (-> img
             (assoc :w (Math/ceil (* (:orig-w img) adjust-ratio))) 
             (assoc :h (Math/ceil (* (:orig-h img) adjust-ratio)))
             (update-in [:x] #(Math/ceil (* % adjust-ratio)))
             )) row))

(defn build-offset-grid [current-entries window-width]
  (loop [entries  current-entries
         row      []
         row-w    0
         v-offset 0
         acc      []]
    (if (empty? entries) (into acc row)
      (let [{img-w :orig-w img-h :orig-h :as entry} (first entries)]
        (if (or (nil? img-w) (nil? img-h))
          ; nil size found - drop the image
          (recur (rest entries) row row-w v-offset acc)
          ; image ok - place image in grid
          (let [new-entry (assoc entry :x row-w :y v-offset)
                new-row  (conj row new-entry) 
                new-row-w (+ row-w img-w)]
            (if (>= new-row-w window-width)
              ; went over the width of the window
              ; - new row
              ; - adjust size of images to fit justified
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
      (let [sorted-raw-entries (filter #(<= 2 (count (-> % :photos first :alt_sizes)))
                                       (sort-by :timestamp #(compare %2 %1) new-entries))
            sorted-entries (into (:entries db) (map normalize-entry sorted-raw-entries))
            new-entries (build-offset-grid sorted-entries (:window-width db))
            ]
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
          new-entries (build-offset-grid (:entries db) new-width)]
      (println (count (:entries db)) (count new-entries))
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

; subscriptions
; ------------------------------------------------------------------------------

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
  :entries
  (fn 
    [db _]
    (reaction (:entries @db))))

(register-sub
  :window-width
  (fn 
    [db _]
    (reaction (:window-width @db))))

; views
; ------------------------------------------------------------------------------

(defn input-form 
  []
  (let [current-search   (subscribe [:search-term])
        current-val      (atom @current-search)
        maybe-new-search #(when (and (not-empty @current-val)
                                     (not= @current-val @current-search))
                            (dispatch [:new-search @current-val]))]
    (fn []
      [:form {:on-submit (fn [ev]
                           (.preventDefault ev)
                           (maybe-new-search))}
       ; text-input
       [:input {:type "text"
                :value @current-val
                :on-change #(reset! current-val (.. % -target -value))}]

       ; submit-button
       (when (and (not-empty @current-val)
                  (not= @current-val @current-search))
         [:input {:type "button"
                  :value "new search"
                  :on-click maybe-new-search}])]
      )))



(defn grid-entry 
  [{:keys [x y w h url title]}]
  [:li {:style {:left     (str x "px")
                :top      (str y "px") 
                :width    (str w "px")}}
   [:img {:src url 
          :alt title 
          :width (str w "px") 
          :height (str h "px")}]])

(defn entry-list
  []
  (let [entries (subscribe [:entries])]
    (when (not-empty @entries)
      (into [:ul {:id "gridlist"}]
            (mapv grid-entry @entries)))))

(defn header
  []
  (let [search-term (subscribe [:search-term])
        mode (subscribe [:mode])]
    [:div {:id "header"}
     (if (empty? @search-term)
       [:h1 "Enter Search:"]
       [:h1 "Current Search: " @search-term])
     [input-form]
     (case @mode
       :loading [:h2 "loading"]
       :finished [:h2 "Found All Entries."]
       "")]))

(defn application 
  []
  (fn [] [:div [header] [entry-list]]))

; run
; ------------------------------------------------------------------------------

(defonce intialize
  (do
    (.addEventListener
      js/window "resize" #(dispatch [:resize]))
    (.addEventListener
      js/window "scroll" #(dispatch [:scroll]))
    (dispatch-sync [:initialize])))

(defn ^:export run
  []
  (reagent/render [application]
                  (js/document.getElementById "app")))

