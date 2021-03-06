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

                 [regexpforobj "1.0.0-alpha3-SNAPSHOT"]
                 [fulab.zarnidict/fulabdsl "1.0.0-alpha1-SNAPSHOT"]


                 [com.ashafa/clutch "0.4.0"]
                 [io.aviso/pretty "0.1.24"]
                 [aprint "0.1.3"]

                 [com.ashafa/clutch "0.4.0"]
                 [clj-http "2.1.0"]
                 [org.clojure/data.json "0.2.6"]
                 [com.rpl/specter "0.10.0"]

                 [org.clojure/java.jdbc "0.6.1"]
                 [org.postgresql/postgresql "9.4-1201-jdbc41"]

                 [dk.dren/hunspell "1.3.2-SNAPSHOT"]
                 [net.mikera/core.matrix "0.52.0"]
                 [org.clojure/tools.cli "0.3.5"]

                 ]
  :main fulabdicts.main)
