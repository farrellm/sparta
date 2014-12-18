(ns sparta.client
  (:require [clojure.core.async :as async :refer [>! <!! alts!! chan go thread]]
            [clojure.java.shell :refer [sh]])
  (:import [org.rosuda.REngine.Rserve RConnection RserveException]))

(def ^:dynamic *r-cmd* "/usr/bin/R")
(def ^:dynamic *port* 6311)

(defn connect [port]
  (if-let [conn (try
                  (RConnection. "localhost" port)
                  (catch RserveException e
                    (Thread/sleep 10)))]
    conn
    (recur port)))

(defn start
  ([] (start *port*))
  ([port]
   (let [c1 (chan)
         c2 (chan)]
     (go (>! c1 (sh *r-cmd* "-e" (str "library(Rserve); Rserve(port=" port ", args='--vanilla')"))))
     (go (>! c2 (connect port)))
     (let [[v c] (alts!! [c1 c2])]
       (if (= c c1)
         {:exit-state v}
         {:port port
          :connection v
          :exit-state-chan c1})))))

(defn shutdown [r]
  (.shutdown (:connection r)))

(comment
  (def r (start))
  (shutdown r)
  (<!! (:exit-state-chan r)))
