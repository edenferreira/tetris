(defproject tetris "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [quil "3.0.0"]]

  :aliases {"kaocha" ["with-profile" "+kaocha" "run" "-m" "kaocha.runner"]
            "test"   ["kaocha"]}

  :profiles {:dev {:dependencies [[lambdaisland/kaocha "0.0-541"]]
                   :source-paths ["dev"]}
             :kaocha {:dependencies [[lambdaisland/kaocha "0.0-541"]]}})
