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

(def image-width 500)

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

(register-handler 
  :continue-search
  (fn [db [_]]
    (perform-search (:search-term db) (:before db))
    (assoc db :mode :loading)))

(register-handler 
  :search-result
  (fn [db [_ new-entries]]
    (if (not-empty new-entries)
      (let [sorted-entries (sort-by :timestamp #(compare %2 %1) new-entries)]
       (-> db
          (update-in [:entries] into sorted-entries)
          (assoc :before (-> sorted-entries last :timestamp))
          (update-in [:page] inc)
          (assoc :mode :loaded)
          ))
      (-> db (assoc :mode :finished)))))

(register-handler 
  :resize
  (fn [db [_]]
    (assoc db :window-width 
           (.. js/window -innerWidth)
           )))

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

(defn build-offset-grid [current-items window-width]
  (let [col-n (inc (Math/floor (/ window-width image-width)))
        col-w (/ window-width col-n)]
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
              (recur
                (rest items)
                (conj coll new-item)
                (inc idx)
                new-offsets))))))))

(defn grid-entry 
  [{:keys [title photo post-url x y w]}]
  [:li {:style {:left     (str x "px")
                :top      (str y "px") 
                :width    (str w "px")}}
   [:img {:src (photo :url)}]])

(defn entry-list
  []
  (let [entries (subscribe [:entries])
        window-width (subscribe [:window-width])]
    (when (not-empty @entries)
      (let [grid-entries (build-offset-grid @entries @window-width)]
         (into [:ul {:id "gridlist"}]
               (mapv grid-entry grid-entries))))))

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

