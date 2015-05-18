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
  {:mode        :search
   :search-term ""
   :entries     []
   :page         0
   :window-width (.. js/window -innerWidth)
   :error-string ""
   })

(def image-width 500)

(defn perform-search [search-term before]
  (.send (Jsonp. (Uri. "http://api.tumblr.com/v2/tagged")) 
         (clj->js 
           {:tag     search-term
            :before  before
            :api_key "pekKZHs4hKvshK1NRyXlawVhO203uYg0MMfGj5Tq8ts6M1Wq9Z"})
         ; response handler
         (fn [v]
           (case (.. v -meta -status)
             200 
             (dispatch
               [:search-result 
                (filter (fn [item] (= (:type item) "photo"))
                        (js->clj (.. v -response) :keywordize-keys true))])
             ; error from api
             (dispatch [:error (str "API Error: " (js->clj (.. v -meta -msg) :kewordize-keys true))])))
         ; error handler
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
  :search
  (fn [db [_ search-term before]]
    (perform-search search-term before)
    (assoc db :search-term search-term
              :mode        :loading
              :entries     []
              :page        0
              )))

(register-handler 
  :search-result
  (fn [db [_ new-entries]]
    (-> db
        (update-in [:entries] into new-entries)
        (update-in [:page] inc)
        (assoc :mode :loaded))))

(register-handler 
  :resized
  (fn [db [_]]
    (assoc db :window-width 
           (.. js/window -innerWidth)
           )))

; subscriptions
; ------------------------------------------------------------------------------

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
  (let [current-search (subscribe [:search-term])
        current-val (atom @current-search)] 
    (fn []
      [:form {:on-submit (fn [ev]
                           (.preventDefault ev)
                           (dispatch [:search @current-val 0]))}
       [:input {:type "text"
                :value @current-val
                :on-change #(reset! current-val (.. % -target -value))}]
       [:input {:type "button"
                :value "submit"
                :on-click #(dispatch [:search @current-val 0])}]
       ])))

(defn- build-offset-grid [current-items window-width]
  (let [col-n (inc (Math/floor (/ window-width image-width)))
        col-w (/ window-width col-n)
        ]
    (loop [items   current-items
           coll    []
           idx     0
           offsets (zipmap (range col-n) (repeat 0))]
      (if (empty? items) coll
        (let [item  (first items)
              title (clojure.string/replace (:slug item)  #"-"  " ") 
              photo (-> item :photos first :alt_sizes second)]
          (if (or (nil? (:height photo)) (nil? (:width photo)))
            ; nil size found - drop the image
            (recur (rest items) coll idx offsets)
            ; image ok - place image in grid
            (let [offset-n    (mod idx col-n)
                  offset      (apply min-key second offsets)
                  offset-x    (* col-w (first offset))
                  offset-y    (second offset)
                  new-height  (int (* (:height photo) (/ col-w (:width photo))))
                  new-offsets (update-in offsets [(first offset)] + new-height)
                  new-item {:index idx
                            :title title
                            :photo photo
                            :post-url (:post_url item)
                            :x offset-x
                            :y offset-y
                            :w col-w
                            }]
              (println offsets)
              (recur
                (rest items)
                (conj coll new-item)
                (inc idx)
                new-offsets))))))))

(defn grid-entry [{:keys [title photo post-url x y w]}]
  [:li {:style {:list-style "none"
                :position :absolute
                :left     (str x "px")
                :top      (str y "px") 
                :width    (str w "px")}}
   [:img 
    {:src (photo :url)
     :style {:width (str w "px")
             :display "block"
             }
     }]])

(defn entry-list 
  []
  (let [entries (subscribe [:entries])
        window-width (subscribe [:window-width])]
    (when (not-empty @entries)
      (let [grid-entries (build-offset-grid @entries @window-width)]
        (println (pr-str grid-entries))
         (into [:ul {:style {:margin-top "50px" :position "relative"}}]
               (mapv grid-entry grid-entries))))))

(defn application 
  []
  (let [search-term (subscribe [:search-term])]
    (fn []
      [:div
       [:h1 @search-term]
       [input-form]
       [entry-list]
       ])))

; run
; ------------------------------------------------------------------------------

(defn ^:export run
  []
  (dispatch-sync [:initialize])
  (.addEventListener
    js/window "resize" #(dispatch [:resized]))
  (reagent/render [application]
                  (js/document.getElementById "app")))
