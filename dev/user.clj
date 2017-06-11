(ns user
  (:require
   [figwheel-sidecar.repl-api :as f]
   [aleph.http :as http]
   [xom.server :as server]
   [datomic.api :as d]
   ))

(def server (atom nil))
(defn start
  []
  (f/start-figwheel! {:builds [{:id :dev
                                :source-paths ["src"]
                                :figwheel true
                                :compiler {:main 'xom.ui
                                           :asset-path "js/compiled/out"
                                           :output-to "resources/public/js/compiled/xom.js"
                                           :output-dir "resources/public/js/compiled/out"
                                           :source-map-timestamp true
                                           }
                                }]})
  (reset! server (http/start-server #'server/my-app
                     {:port 3000}))
  (when (d/create-database "datomic:mem://xom")
    @(d/transact (server/conn) server/schema)))

(defn stop
  []
  (f/stop-figwheel!)
  (.close @server)
  (reset! server nil)
  (d/delete-database "datomic:mem://xom"))

(defn random-move
  [p]
  {:pos/row (rand-int 3)
   :pos/col (rand-int 3)
   :pos/mark p})

(defn random-valid-move
  [g]
  (let [mark (server/player-turn g)]
    (loop [m (random-move mark)]
      (if (server/valid-move? g m)
        m
        (recur (random-move mark))))))

(defn apply-move
  [g m]
  (update g :xom/positions (fnil conj []) m))

(defn random-game-seq
  ([g]
   (lazy-seq
    (if (= :none (server/winner (server/board g)))
      (let [m (random-valid-move g)]
        (cons g (random-game-seq (apply-move g  m))))
      (cons g nil)))))

(defn parse-val
  [v]
  (try
    (Integer/parseInt v)
    (catch Exception ex
      (println "Enter a number!"))))

(defn prompt-move
  []
  (print "Enter a move: ")
  (flush)
  (map parse-val (clojure.string/split (read-line) #"\s+")))

(defn cel-str
  [v]
  (case v
    :x "X"
    :o "O"
    " "
    ))

(defn print-row
  [[a b c]]
  (println "" (cel-str a) "|" (cel-str b) "|" (cel-str c))
  )

(defn print-sep
  []
  (println "---+---+---")
  )

(defn print-board
  [b]
  (let [[t m b] b]
    (print-row t)
    (print-sep)
    (print-row m)
    (print-sep)
    (print-row b)
    )
  )

(defn console-game
  [player-name player]
  (loop [g (server/create-game) msg nil]
    (let [b (server/board g)
          winner (server/winner b)
          turn (server/player-turn g)]
      (print-board b)
      (when msg
        (println msg))
      (if (= :none winner)
        (if (= turn player)
          (let [[r c] (prompt-move)
                m {:pos/row r
                   :pos/col c
                   :pos/mark player}]
            (if (and r c (server/valid-move? g m))
              (recur (apply-move g m) (str "You moved to " r ", " c))
              (recur g (str "Invalid move: " [r c]))))
          (let [m (random-valid-move g)]
            (recur (apply-move g m) (str "Computer moved to " (:pos/row m) ", " (:pos/col m)))))
        (do
         (cond
          (= player winner)
          (println "You Won!!!")

          (= :cat winner)
          (println "Cat got it!!")

          :else
          (println "You lost!!")))))))

(comment

 (map (comp server/winner server/board) (random-game-seq (server/create-game "bob" "alice")))

 (print-board (server/board (first (random-game-seq (server/create-game "bob" "alice")))))
 (doseq [g (random-game-seq (server/create-game "bob" "alice"))]
   (print-board (server/board g))
   )


(->> board flatten (remove nil?) count)

  (start)
  (stop)
  )
