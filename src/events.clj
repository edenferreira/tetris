(ns events)

(def log (atom []))

(defn start! [payload state]
  (reset! log
          [[:event.type/start payload state]])
  state)

(defn new! [event payload state]
  (swap! log
         conj
         [event payload state])
  state)

(defn dump! [name]
  (spit (str "./events-" name ".edn")
        (prn-str @log)))