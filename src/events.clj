(ns events)

(def log (atom []))

(defn start-events! [payload state]
 (swap! events 
        (constantly
         [:event.type/start payload state]))

(defn new-event! [event payload state]
 (swap! events conj 
               [event
                payload
                state])
 state)

(defn dump-events! [name]
  (spit (str "./events-" name ".edn") (prn-str @log)))