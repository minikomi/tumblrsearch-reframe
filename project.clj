(defproject tumblr-reframe "0.1.0-SNAPSHOT"
  :description "FIXME: write this!"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.clojure/core.async "0.2.374"]
                 ; app specific
                 [re-frame "0.5.0"]
                 [reagent "0.5.1"]
                 [secretary "1.2.3"]
                 ]

  :plugins [[lein-cljsbuild "1.1.1"]
            [lein-figwheel "0.5.0-2"]]

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"]
  
  :cljsbuild {
    :builds [{:id "dev"
              :source-paths ["src"]

              :figwheel {:on-jsload "tumblr-reframe.core/run"}

              :compiler {:main tumblr-reframe.core
                         :asset-path "js/compiled/out"
                         :output-to "resources/public/js/compiled/tumblr_reframe.js"
                         :output-dir "resources/public/js/compiled/out"
                         :source-map-timestamp true }}
             {:id "min"
              :source-paths ["src"]
              :compiler {:output-to "resources/public/js/compiled/tumblr_reframe.js"
                         :main tumblr-reframe.core
                         :optimizations :advanced
                         :pretty-print false}}]}

  :figwheel {
             :css-dirs ["resources/public/css"]
             :nrepl-port 7888 })
