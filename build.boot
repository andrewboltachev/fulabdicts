#!/usr/bin/env boot


(set-env!
  :project      'fulabdicts
  :version      "0.1.0-SNAPSHOT"
  :dependencies '[
                  [ns-tracker "0.2.2"]
                  [adzerk/boot-reload        "0.2.6"]
                  [fipp "0.5.2"]

                  [regexpforobj "1.0.0-alpha2-SNAPSHOT"]
                  [fulab.zarnidict/fulabdsl "1.0.0-alpha1-SNAPSHOT"]


                  ]
  :source-paths    #{"src"}
  )

(use '[ns-tracker.core :only [ns-tracker]])
(require '[adzerk.boot-reload :refer [reload]])
(require '[fulabdicts.core])
(require '[fulab.zarnidict.fulabdsl.core :as fulabdsl])
(require '[fulabdicts.structures])
(require '[regexpforobj.core])
(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])
(require '[clojure.java.io :as io])
(require '[boot.core :as c])


(deftask wrap-reload
  "Reload namespaces of modified files before the request is passed to the
  supplied handler.
  Accepts the following options:
 :dirs - A list of directories that contain the source files.
          Defaults to [\"src\"]."
  ;{:arglists '([handler] [handler options])}
  ;[handler & [options]]
  [d dirs [str] "Dirs"]
  (let [source-dirs (or dirs ["src"])
        modified-namespaces (ns-tracker source-dirs)]
    (fn [handler]
      (fn [request]
      (doseq [ns-sym (modified-namespaces)]
        (require ns-sym :reload))
      (handler request)))))

(deftask main
  []
  (with-pre-wrap fileset
    (fulabdicts.core/main)
    fileset
    )
  )


(deftask dev
  "Development loop"
  []
  (comp
    (watch)
    (speak)
    (reload)
    (wrap-reload "src")
    (main)
    )
  )


(defn upload-fn [filename language-in language-out structure process-fn]
  (if (not-any? nil? (hash-set structure language-in language-out))
    (if-let [structure-obj (fulabdicts.structures/structures structure)]
      (let [text (slurp filename)
            lines (clojure.string/split-lines text)
            _ (println "Read file" (str\" filename \" ":") (count lines) "lines")
            parsed (apply fulabdsl/parse-fulabdsl-lines-short lines structure-obj)
            ]
        (if (regexpforobj.core/is_parsing_error? parsed)
          (do
          (println "ParsingError")
            (fipp
              (update parsed :context
                   #(apply str (take 1000 (prn-str %)))
                   )))
          (let [_ (println "Pasring success:" (count parsed) "articles")]
            (process-fn parsed)
            )
          )
        )
        (println "Structure" (str \" structure \") "is not supported!")
      )
    (println "Error: You must specify structure, language-in, language-out")
    )
  )


(defn upload-fn-short [filename language-in language-out structure]
  (if (not-any? nil? (hash-set structure language-in language-out))
    (if-let [structure-obj (fulabdicts.structures/structures structure)]
      (let [text (slurp filename)
            lines (clojure.string/split-lines text)
            _ (println "Read file" (str\" filename \" ":") (count lines) "lines")
            parsed (apply fulabdsl/parse-fulabdsl-lines-short lines structure-obj)
            ]
        (if (regexpforobj.core/is_parsing_error? parsed)
          (do
          (println "ParsingError")
            (fipp
              (update parsed :context
                   #(apply str (take 1000 (prn-str %)))
                   )))
          (let [_ (println "Pasring success:" (count parsed) "articles")]
            parsed
            )
          )
        )
        (println "Structure" (str \" structure \") "is not supported!")
      )
    (println "Error: You must specify structure, language-in, language-out")
    )
  )


(deftask upload
  "Parse dict and upload it to the web-service"
  [f filename FILE str "Filename"
   i language-in NAME str "Input language ISO code"
   o language-out NAME str "Output language ISO code"
   s structure NAME str "Название структуры"
   ]
  (with-pre-wrap fileset
    (upload-fn
      filename
      language-in
      language-out
      structure
      identity)
    fileset
    )
  )


(deftask upload-dev
  "Parse dict and output line count if it's ok. In loop"
  [f filename FILE str "Filename"
   i language-in NAME str "Input language ISO code"
   o language-out NAME str "Output language ISO code"
   s structure NAME str "Название структуры"
   ]
  (let [tmp (c/tmp-dir!)]
    (comp
    (fn middleware [next-handler]                   ; [2]
      (fn handler [fileset]                         ; [3]
        (c/empty-dir! tmp)
        (let [out-file (io/file tmp "file1")]
(doto out-file
    io/make-parents
    (spit
      (prn-str (doall (upload-fn-short
      filename
      language-in
      language-out
      structure
        )))
      ))
          )
        (next-handler
          (c/add-resource fileset tmp))))
      (watch)
      (speak)
      (reload)
      (wrap-reload "src")
    (with-pre-wrap fileset
      #_(upload-fn-short
        filename
        language-in
        language-out
        structure
        identity)
      (doseq [in (c/output-files fileset)]
        (when (= (c/tmp-path in) "file1")
          (println
            (doall (slurp (c/tmp-file in)))
            )
          )
        )
      fileset
      ))
    )
  )
