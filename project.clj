(defproject cljds/ch9 "0.1.0"
  :description "Example code for the book Clojure for Data Science"
  :url "https://github.com/clojuredatascience/ch9-time-series"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [incanter "1.5.6"]
                 [clj-time "0.9.0"]
                 [succession "0.1.0"]
                 [clatrix "0.4.0"]
                 [org.apache.commons/commons-math3 "3.4.1"]]
  :profiles {:dev
             {:dependencies [[org.clojure/tools.cli "0.3.1"]]
              :repl-options {:init-ns cljds.ch9.examples}}}
  :main cljds.ch9.core)
