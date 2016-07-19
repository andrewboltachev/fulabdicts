(ns fulabdicts.main
  (:require
    [clojure.core.async :as async :refer [<! >! timeout chan alt! alts! go go-loop put!
                                          sliding-buffer
                                          ]]
    [clojure.java.io :as io]
    [clojure-watch.core :refer [start-watch]]
    [fulab.zarnidict.fulabdsl.core :as fulabdsl]
    [regexpforobj.core :as regexpforobj]
    [regexpforobj.main :as regexpforobj-main]

    [io.aviso.ansi :as font]

    ;[clj-http.client :as client]
    [clojure.java.jdbc :as jdbc]
    [clojure.core.reducers :as r]
    [clojure.tools.cli :refer [parse-opts]]
    )
  (:use
    [com.rpl.specter]
    )
  )
(use 'aprint.core)
(require '[fipp.edn :refer (pprint) :rename {pprint fipp}])
(require 'fulabdicts.metadata)
(require 'fulabdicts.tokenizers)
(require 'fulabdicts.grammars) ; TODO: split file


(defn ifipp [x]
  (do
    (fipp x)
    x)
  )

(defmacro with-ns
  "Evaluates body in another namespace.  ns is either a namespace
  object or a symbol.  This makes it possible to define functions in
  namespaces other than the current one."
  [ns & body]
  `(binding [*ns* (the-ns ~ns)]
     ~@(map (fn [form] `(eval '~form)) body)))

(defn truncate
  [s n]
  (apply str (take n s)))

(defn prn2 [data]
  (str (truncate (prn-str data) 150) "... (" (-> data class str) ")")
  )

(defmacro watch-file-for-changes-depreciated [filename output-channel & [payload]]
  `(do
  (let [~'file-ch (chan
    (sliding-buffer 1)
  )
        ~'file (io/file ~filename)]
    (do
#_(println "go-loop" filename)
(go-loop [~'old-filename nil]
         (let [[[~'event ~'filename :as ~'v] ~'port] (alts! [~'file-ch
                                (timeout 500)
                                ]
                               :priority true
                               )]
           (when-not (nil? ~'old-filename)
             #_(println
                 (str "!!!!!!!!!!!!!!!!!!!!!!!!!!!!" ~'v ~'port)
                 (System/currentTimeMillis)
               )
              (when
                  (nil? ~'v)
                  (println font/blue-font "File" (str \" ~'old-filename \") "changed" font/reset-font)
                  (put! ~output-channel [~'old-filename ~payload])
                )
               )
             (recur ~'filename)
             )
           )


  (put! ~'file-ch [:initial ~filename])
  (start-watch [{:path (.getParent ~'file)
                 :event-types [:create :modify
                               ;:delete
                               ]
                 :bootstrap (fn [~'path] (println font/blue-font "Starting to watch " ~'path font/reset-font))
                 :callback (fn [~'event ~'filename]
                             (when (= (.getName ~'file) (.getName (io/file ~'filename)))
                               (put! ~'file-ch [~'event ~'filename])
                             )
                             )
                 :options {:recursive false}}])
    )
     ))
  )

(def input-file-ch (chan))
(def tokenizer-file-ch (chan))
(def params-file-ch (chan))

(def input-file-data-ch (chan))
(def tokenizer-data-ch (chan))
(def params-file-data-ch (chan))

(def result-ch (chan))

(go-loop []
         (let [
               [filename {:keys [structure-name] :as options}] (<! input-file-ch)
            data
                 (try
                  (let [data
                    (with-open [rdr (clojure.java.io/reader filename)]
                      (doall (line-seq rdr))
                      )]

                    #_(apply
                      fulabdsl/parse-fulabdsl-lines-short
                      data
                      (get fulabdicts.tokenizers/data structure-name)
                      )
                    data

                    )
                 (catch Throwable e (do (println e)
                                        [] ; XXX: empty data
                                        ))
                 )
               ]
           (println "Sending data off the channel"
                    ;(prn2 data)
                    )
           (put! tokenizer-data-ch data)
           )
         (recur)
         )

(go-loop
  [
   input nil
   params nil

   await-for-changes false
   ]
  (let
    [
     [value port]
     (if (or (empty? input) (empty? grammar) await-for-changes)
       (do
         ;(println "waiting for changes")
         ;(println input)
         ;(println grammar)
         (alts! [tokenizer-file-ch tokenizer-data-ch])
         )
       (do
         ;(println "just checking if there's something new")
         (alts! [tokenizer-file-ch tokenizer-data-ch] :default nil)
         )
       )
     ]
    (cond
      (= port tokenizer-file-ch)
      ;
      (do
        (println "Parsing tokenized data...")
        (recur
          value
          params
          false
          )
        )

      (= port tokenizer-data-ch)
      ;
      (do
        (recur
          input
          value
          false
          )
        )

      :else
      (do
        (println "Tokenizing and sending data off the ch...")
        (let [result
              (try
                (apply fulabdsl/parse-fulabdsl-lines-short input params)
                (catch Throwable e {:error :tokenizer-runtime
                                    :context (prn-str e)})
                )
              ]
          (if
            (regexpforobj/is_parsing_error? result)
            (do
              (println font/bold-red-font "Tokenizer error" font/reset-font)
              (prn result)
              (newline)
              )
            (put! input-file-ch result)
            )
          )
        (recur
          input
          params
          true
          )
        )

    )
  )
  )

(go-loop []
         (let [
               [params-filename {:keys [structure-name] :as options}] (<! params-file-ch)

               data (do
                      (require 'fulabdicts.grammars :reload)
                      (get fulabdicts.grammars/data structure-name)
                   )
               
               ]
           (println "Sending grammar off the channel"
                    (prn2 data)
                    )
           (put! params-file-data-ch [options data])
           )
         (recur)
         )

(go-loop
  [
   input nil
   params nil

   current-input nil
   result []

   await-for-changes false
   ]
  (let
    [
     [{:keys [structure-name] :as options} grammar] params
     folding (not (-> options :cli-options :no-folding))
     [value port]
     (if (or (empty? input) (empty? grammar) await-for-changes)
       (do
         ;(println "waiting for changes")
         ;(println input)
         ;(println grammar)
         (alts! [input-file-data-ch params-file-data-ch])
         )
       (do
         ;(println "just checking if there's something new")
         (alts! [input-file-data-ch params-file-data-ch] :default nil)
         )
       )
     ]
    (cond
      (= port input-file-data-ch)
      (recur
        value
        params

        value
        []
        false
        )

      (= port params-file-data-ch)
      (recur
        input
        value

        input
        []
        false
        )

      (empty? current-input)
      (let [
            ]
        ;(println "Result")
        (put! result-ch [options result])
        (recur
          input
          value

          input
          []
          false
          )
        )

      :else
      (let
        [
         _ (println
             "will apply" 
             )
         _ (when (regexpforobj/is_parsing_error? current-input)
             (println font/bold-red-font "Tokenizer error" font/reset-font)
              (aprint current-input)
             )
         article (first current-input)
         [word body] article
         
         grammar-applied 
         (binding [regexpforobj/*regexpforobj-debug1* false]
         (time (regexpforobj/run grammar body))
           )

         grammar-applied (if
                           (regexpforobj/is_parsing_error?
                             grammar-applied)
                           grammar-applied
                           (if folding
                             (let [r 
                           (clojure.walk/postwalk
                             (fn [x]
                               (if (and (map? x) (fn? (:payload x)))
                                 ((:payload x) x)
                                 
                                 x
                                )
                               )
                             
                              grammar-applied)]
                               (if (:clean-grammar (get fulabdicts.metadata/data structure-name))
                                 (clojure.walk/postwalk
                                 (fn [x]
                                   (if (map? x)
                                   (cond-> x
                                     (= (:type x) :InputChar)
                                     :payload
                                     (= (:type x) :SeqNode)
                                     :value)
                                   x
                                   )
                                   ) r) r)
                               )
                             grammar-applied
                             )
                           )


         ]
        (if (regexpforobj/is_parsing_error? grammar-applied)
          (do
            _ (clojure.pprint/pprint
                (vec
                  body
                  )
                )
            (println font/bold-red-font word "Error")
            (aprint (update-in grammar-applied [:context :tail] regexpforobj/grammar_pretty))
            (recur
              input
              params
  
              input
              []
              true
              )
            )
          (do
            (println font/bold-black-font word "OK" font/reset-font)
            (recur
              input
              params

              (rest current-input)
              (conj result [word grammar-applied])
              false
              )
            )
          )
        )

      )
    )
  )

; (aprint (grammar_pretty (Or (reduce (fn [a b] (concat a (cons (Seq [ b examples ])  (map #(Star (Seq [b %])) a) ))) [] (reverse [trn1 trn2 trn])))))


(go-loop []
         (let [[{:keys [structure-name] :as options} result] (<! result-ch)
               folding (-> options :cli-options :no-folding)
               ]
           (Class/forName "org.postgresql.Driver") ; TODO figure out this
           (jdbc/with-db-connection [db-spec 
                                     ;{:connection-uri
                                    (System/getenv "ZARNIDICT_DATABASE_URL")
                                              ;}
                                     ]
             (let [m (get fulabdicts.metadata/data structure-name)
                   _ (println font/blue-font "Creating dictionary in the DB..." font/reset-font)
                   dt (new java.util.Date)
                   dt-s (java.sql.Date. (.getTime dt))
              r (jdbc/insert! db-spec :dicts_dictionary
                              {:name (str structure-name " at " dt)
                               :lang_in_id (:lang_in m)
                               :lang_out_id (:lang_out m)
                               :complete false
                               :structure structure-name
                               :date_added dt-s
                               }) ;; Create
                   dictionary (first r)
                   _ (println dictionary)

                   get-or-create-article (fn [dictionary word]
                                           (let [existing_one
                                                 (jdbc/query db-spec
                                                             ["SELECT id FROM dicts_article WHERE dictionary_id = ? AND name = ?" (:id dictionary) word])
                                                 existing_one_id (-> existing_one first :id)
                                                 ]
                                             (if existing_one_id
                                               existing_one_id
                                               (-> (jdbc/insert!
                                                 db-spec
                                                     :dicts_article
                                                 {:dictionary_id (:id dictionary)
                                                  :name word
                                                  }
                                                 ) first :id)
                                               )
                                               
                                             )
                                           )
                   ]
              (doseq [[word body] result]
                (println word)
                (let [article-id
                  (get-or-create-article dictionary word)
                  ]
                  article-id
                  (-> (jdbc/insert!
                      db-spec
                          :dicts_articleversion
                      {:article_id article-id
                       :body (prn-str (clojure.walk/postwalk
                                        (fn [x]
                                          (when-not (fn? x) x))
                                        body))
                       :date_added dt-s
                       :action true
                       :user_id nil
                       :folding (some? folding)
                       }
                      ) first :id)
                (aprint
                  (->>
                    body
                    (clojure.walk/postwalk
                      (fn [x]
                        (if (and (map? x) (fn? (:payload x)))
                          (dissoc x :payload)
                          x)
                        )
                      )
                    ;regexpforobj/grammar_pretty
                    )
                  )
                (newline)
                ))
               (jdbc/update! db-spec :dicts_dictionary {:complete true} ["id = ?" (:id dictionary)]) ;; Update
               ))
           )
         (recur)
         )

(defn get-dictionary-by-id [db-spec dictionary-id]
  (let [articles-result (jdbc/query db-spec ["SELECT \"dicts_article\".\"id\", \"dicts_article\".\"dictionary_id\", \"dicts_article\".\"name\" FROM \"dicts_article\" WHERE \"dicts_article\".\"dictionary_id\" = ?" dictionary-id])
        get-article-version (fn [article-id] (first (jdbc/query db-spec ["SELECT \"dicts_articleversion\".\"id\", \"dicts_articleversion\".\"article_id\", \"dicts_articleversion\".\"body\", \"dicts_articleversion\".\"date_added\", \"dicts_articleversion\".\"user_id\", \"dicts_articleversion\".\"action\" FROM \"dicts_articleversion\" WHERE \"dicts_articleversion\".\"article_id\" = ? ORDER BY \"dicts_articleversion\".\"date_added\" DESC, \"dicts_articleversion\".\"id\" DESC LIMIT 1" article-id])))
        ]
    (map
      (fn [article]
        (let [article-version (get-article-version (:id article))]
        {:id (:id article-version)
         :name (:name article)
         :body (try
                 (read-string (:body article-version))
                 (catch Throwable e
                   (do
                     (println (:body article-version))
                     (println e)
                     ))
                 )
         }
        ))
      articles-result)
    )
  )

(def debug-ch (chan 10))

(require 'clojure.core.matrix)

(go-loop [[color :as colors] [
                              font/red-font
                              font/blue-font
                              font/green-font
                              ]]
         (let [msg (<! debug-ch)]
           (println color msg font/reset-font) 
           (newline)
           (recur
             (clojure.core.matrix/rotate colors 0 1)
             )
           )
         )

(defn -main [& args]
  ; check exists (.exists (io/file "filename.txt"))
  (let [
        cli-options [[
                      nil "--no-folding"
                      ]]
        opts-map (parse-opts args cli-options)
        args (:arguments opts-map)
        _ (prn opts-map)
        num-args (partial = (count args))
        ]
  (cond
    (num-args 2)
    (let [
          [input-file structure-name] args
          ]
      (watch-file-for-changes-depreciated
        input-file
        input-file-ch
        {:structure-name structure-name
         :cli-options opts-map}
        )
      (watch-file-for-changes-depreciated
        "src/fulabdicts/tokenizers.clj"
        tokenizer-file-ch
        {:structure-name structure-name
         :cli-options opts-map}
        )
      (watch-file-for-changes-depreciated
        "src/fulabdicts/grammars.clj"
        params-file-ch
        {:structure-name structure-name
         :cli-options opts-map}
        )
      )
    (num-args 4)
    (do
      (Class/forName "org.postgresql.Driver") ; TODO figure out this
      (jdbc/with-db-connection [db-spec 
                              (System/getenv "ZARNIDICT_DATABASE_URL")]
                                
    (let [
          [dictionary-id wordlist-id hunspell-filename-base structure-name] args
          dictionary-id (Integer/parseInt dictionary-id)
          wordlist-id (Integer/parseInt wordlist-id)
          dictionary (get-dictionary-by-id db-spec dictionary-id)
          wordlist (get-dictionary-by-id db-spec wordlist-id)

          hunspell (.getDictionary (dk.dren.hunspell.Hunspell/getInstance) hunspell-filename-base)
          stems (fn [word]
                 (apply hash-set
                        (.stem hunspell word)
                        )
                 )
          word-matches (fn [master-stems word]
                         (not (empty? (clojure.set/intersection
                                        master-stems
                                        (stems word)
                                        )))
                        )
          
          _ (println "trns")
          trns (time (doall (map (fn [article]
               (update-in article [:body]
                          (fn [body]
                            ; ...
                            (doall (filter #(and (map? %) (contains? % :trn)) (tree-seq #(or (sequential? %) (map? %)) #((if (map? %) vals identity) %) body)))
                            )))

        dictionary)))

          path0 (comp-paths :body ALL :examples)

          ;trns-match
          #_(map (fn [article]
                            (transform path0
                                   ; ...
                                       (fn [examples]
                                   (map (fn [{:keys [mhr rus aut] :as example}]
                                                             ; ...
                                                             (let [rus1 (apply str rus)
                                                                   rus-words
                                                                   (clojure.string/split rus1 #"[^а-яёА-ЯЁ\-]")
                                                                   ]
                                                                 (assoc example :match (apply clojure.set/union (map stems rus-words)))
                                                                   ))

                                        examples
                                        ))
                                   article)
                            ) trns)

          path1 (comp-paths :body ALL :examples)
          path2 (comp-paths ALL :body ALL :examples ALL)

          examples1 (time (doall (pmap
                     (fn [{:keys [mhr rus aut] :as example}]
                            ; ...
                            (let [rus1 (apply str rus)
                                  rus-words
                                  (clojure.string/split rus1 #"[^а-яёА-ЯЁ\-]")
                                  ]
                              ;(put! debug-ch example)
                              (zipmap
                                (apply clojure.set/union (map stems rus-words))
                                (repeat [example])
                                )
                              ))
                     (select path2 trns)
                     )))


          _ (println "examples1" (time (count examples1)))
          ;_ (println (take 5 examples1))

          _ (println "words1")
          words1 (time (r/fold
                   (partial merge-with #(reduce conj (vec %1) %2))
                   examples1
                   ))

          _ (println (count words1))
          _ (println (-> words1 keys first))
          _ (println (-> words1 vals first))

          perevertysh
          (time (doall (pmap
                        (fn [x]
                          (let [word (:name x)
                                master-stems (stems word)
                                ;this-word-matches (partial
                                ;                    word-matches
                                ;                    (stems x)
                                ;                    )
                                matching-examples (time (do
                                                       (put! debug-ch (str "matching examples " word))
                                                       (doall
                                                         (into
                                                           []
                                                           (mapcat (fn [stem]
                                                                     (get words1 stem)
                                                                     ))
                                                           master-stems
                                                         )
                                                         )))
                                ]
                          {:name (:name x)
                           :body {
                                  :original (:body x)
                                  ;:translations translations
                                  :examples matching-examples
                                  }
                           })
                          )
                        wordlist
                        )))
          ]
      









(let [m (get fulabdicts.metadata/data structure-name)
                   _ (println font/blue-font "Creating dictionary in the DB..." font/reset-font)
                   dt (new java.util.Date)
                   dt-s (java.sql.Date. (.getTime dt))
              r (jdbc/insert! db-spec :dicts_dictionary
                              {:name (str structure-name " at " dt)
                               :lang_in_id "rus" ; TODO
                               :lang_out_id "mhr" ; TODO
                               :complete false
                               :structure structure-name
                               :date_added dt-s
                               }) ;; Create
                   dictionary (first r)
                   _ (println dictionary)

                  get-existing-articles (fn [dictionary]
                                          
                                          (into {}
                                                (map (fn [{:keys [id name]}]
                                                       [name id]
                                                       )
                                                       (jdbc/query
                                            db-spec
                                            ["SELECT id FROM dicts_article WHERE dictionary_id = ?" (:id dictionary)]
                                            ))
                                          ))
                   existing-articles (get-existing-articles dictionary)

                   get-or-create-articles (fn [dictionary existing-articles articles]
                                           (map (fn [article]
                                            (let [word (:name article)
                                                 existing_one_id (get existing-articles word)
                                                 ]
                                             (assoc article :id (if existing_one_id
                                               existing_one_id
                                               (-> (jdbc/insert!
                                                 db-spec
                                                     :dicts_article
                                                 {:dictionary_id (:id dictionary)
                                                  :name word}
                                                 ) first :id)
                                               ))
                                               
                                             )) articles)
                                           )
                   ]
              (doseq [
                      ;{:keys [name body]}
                      series (partition 20 20 [] perevertysh)]
                (let [articles (get-or-create-articles
                                 dictionary existing-articles series)]
                  (time (-> (jdbc/insert-multi!
                      db-spec
                          :dicts_articleversion
                      (map (fn [article]
                             {:article_id (:id article)
                       :body (prn-str (clojure.walk/postwalk
                                        (fn [x]
                                          (when-not (fn? x) x))
                                        (:body article)))
                       :date_added dt-s
                       :action true
                       :user_id nil
                       :folding true
                       }
                      )
                           articles
                           ))))
                  (doall (map println (map :name series)))
                #_(aprint
                  (->>
                    body
                    (clojure.walk/postwalk
                      (fn [x]
                        (if (and (map? x) (fn? (:payload x)))
                          (dissoc x :payload)
                          x)
                        )
                      )
                    ;regexpforobj/grammar_pretty
                    )
                  )
                (newline)
                ))
               (jdbc/update! db-spec :dicts_dictionary {:complete true} ["id = ?" (:id dictionary)]) ;; Update
               )


















      )))
      :else
      (println "Enter arguments to do something")
     ))
  )
