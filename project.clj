(defproject fulabdicts "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [
                 [org.clojure/clojure "1.7.0"]
                 [org.clojure/core.async "0.2.374"]
                 [fipp "0.5.2"]
                 [clojure-watch "0.1.11"]

                 [regexpforobj "1.0.0-alpha2-SNAPSHOT"]
                 [fulab.zarnidict/fulabdsl "1.0.0-alpha1-SNAPSHOT"]
                 ]
  :main fulabdicts.main)
