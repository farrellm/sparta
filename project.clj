(defproject sparta "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [org.clojure/core.async "0.1.346.0-17112a-alpha"
                  :exclusions [org.clojure/tools.analyzer.jvm]]
                 [org.clojure/core.match "0.3.0-alpha3"]
                 [org.clojure/tools.analyzer "0.6.4"]
                 [rosuda/rserveengine "1.8-0"]
                 [rosuda/rengine "1.8-0"]]
  ;; for rengine
  :repositories {"project" "file:repo"})
