(ns robotrunner
  (:require [clojure.string :as cs]
            [clojure.java.io :refer [as-file]]
            [clj-http.client :as http]
            [clj-http.cookies]
            [clojure.walk :refer [postwalk]]))

;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def directions {0 [ 0  1]
                 1 [ 1  0]
                 2 [ 0 -1]
                 3 [-1  0]})
(def colors [\r \g \b])
(def opcodes [:move :left :right :mark-red :mark-green :mark-blue :f1 :f2 :f3 :f4 :f5])
(def board-size [16 12])

(defrecord state [dir pos board ip stack steps stars])

;;;;;;;;;;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tile-color [^Character tile]
  (java.lang.Character/toLowerCase tile))

(def sub2idx {:f1 0 :f2 1 :f3 2 :f4 3 :f5 4})
(def idx2sub [:f1 :f2 :f3 :f4 :f5])

(defn star? [^Character tile]
  (java.lang.Character/isUpperCase tile))

;;;;;;;;;;; parsing and printing ;;;;;;;;;;;;;;;;;;;;;;

(defn parse-board [s]
  (mapv vec (partition (first board-size) s)))

(defn board2str [{:keys [dir pos board]}]
  (let [board (assoc-in board pos ({0 \> 1 \v 2 \< 3 \^} dir))]
    (cs/join "\n" 
             (keep #(when (not-every? #{\space} %) 
                      (apply str %)) 
                   board))))

(defn print-state [state]
  (clojure.pprint/pprint (dissoc state :board))
  (println (board2str state)))

(defn available-commands [{b :board cmds :allowedCommands subs :subs}]
  (let [colors (-> (->> b (map tile-color) distinct set)
                 (disj \space)
                 (conj nil))
        commands (list* (when (not= 0 (bit-and cmds 1)):mark-red)
                        (when (not= 0 (bit-and cmds 2)) :mark-green)
                        (when (not= 0 (bit-and cmds 4)) :mark-blue)
                        :move :left :right
                        (keep-indexed (fn [i v] (when (> v 0) (idx2sub i))) subs))]
    (for [cmd (remove nil? commands), color colors]
     [cmd color])))

(defn parse [file]
  (let [token (->> file
                slurp
                (re-find #"(?s)var puzzles = \[\{(.*)\}\]")
                second
                cs/trim
                cs/split-lines)
        pairs (map #(map cs/trim (cs/split % #":")) token)
        parsed-pairs (map (fn [[k v]] [(keyword k) (read-string v)]) pairs)
        parsed (into {} parsed-pairs)
        board (parse-board (:board parsed))] (def board board)
    {:allowed-commands (available-commands parsed)
     :pos [(:robotRow parsed) (:robotCol parsed)]
     :dir (:robotDir parsed)
     :board board
     :title (:title parsed)
     :id (:id parsed)
     :subs (remove zero? (:subs parsed))
     :stars (count (filter star? (apply concat board)))
     :non-blank-tiles (count (remove #{\space} (apply concat board)))}))


;;;;;;;;;;; simulator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn turn-left [dir]
  (mod (+ 3 dir) 4))

(defn turn-right [dir]
  (mod (inc dir) 4))


(defn won? [state]
  (zero? (:stars state)))

(defn run-step 
  "Run one simulation step. Takes and returns the simulation state."
  [{:keys [dir pos ip stack program n stars] :as state}]
  (let [tile-path (apply vector :board pos)
        tile (get-in state tile-path)
        star? (star? tile)
        tile (tile-color tile)
        state (-> state
                (assoc-in tile-path tile) ; clear star on current tile
                (update-in [:ip 1] inc); move instruction pointer
                (assoc :n (inc n))
                (update :stars (if star? dec identity))) 
        [op-code op-color] (get-in program ip)]
    (if (and op-color (not= tile op-color))
      state 
      (case op-code
        :move (assoc state :pos (mapv + pos (directions dir)))
        :left (assoc state :dir (turn-left dir))
        :right (assoc state :dir (turn-right dir))
        :mark-red (assoc-in state tile-path \r)
        :mark-green (assoc-in state tile-path \g)
        :mark-blue (assoc-in state tile-path \b)
        nil (run-step (assoc state :ip (first stack) :stack (rest stack))) ; we ran out of opcodes, try to pop from stack
        ;else a function call
        (assoc state
               ;:n n
               :ip [(sub2idx op-code) 0] 
               :stack (cons (:ip state) stack))))))


(defn init-state 
  "Create initial state as specified in the task"
  [{:keys [dir pos board stars]} program]
  (map->state {:dir dir 
               :pos pos 
               :board board 
               :ip [0 0] 
               :n 0
               :stack []
               :stars stars
               :program program}))

(defn valid? 
  "Is the current state valid?"
  [{:keys [dir pos ip program board]}] ;(println "valid?" pos ip program (get-in program ip) (get-in board pos))
  (and (<= 0 (first pos) (second board-size))
       (<= 0 (second pos) (first board-size))
       (not (nil? (get-in program ip)))
       (#{\b \g \r \B \G \R} (get-in board pos))))

(defn update-statistics 
  "Update statistics of the currently running program for the fitness function."
  [stats task state]
  (let [stars (:stars task)
        stars-left (:stars state)
        seen ((fnil conj #{}) (:seen stats) (:pos state))]
    {:seen seen
     :stars-found (/ (- stars stars-left) stars)
     :tiles-visited (/ (count seen) (:non-blank-tiles task))}))

(defn run-program [program task]
  (let [program (postwalk #(if (seq? %) (vec %) %) program) ; ensure all sequences are vectors
        initial-state (init-state task program)]
    (loop [state initial-state, stats {}] 
      (let [stats (update-statistics stats task state)] 
        (cond 
          (> (:n state) 1000) {:status :too-long :state state :stats stats}
          (not (valid? state)) {:status :invalid-move :state state :stats stats}
          (nil? (:ip state)) {:status :no-more-moves :state state :stats stats}
          (won? state) {:status :success :state state :stats stats}
          :else (recur (run-step state) stats))))))

;;;;;;;;;;;;;;;;;;; Genetic Algorithms based solver ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-individual [{subs :subs ac :allowed-commands}]
  (for [n subs :when (pos? n)]
    (repeatedly (rand n) #(rand-nth ac))))

(defn crossover-single [max-length ind1 ind2]
  (let [[left-1 right-1] (split-at (inc (rand (count ind1))) ind1)
        [left-2 right-2] (split-at (inc (rand (count ind2))) ind2)]
    [(take max-length (concat left-1 right-2))
     (take max-length (concat left-2 right-1))]))

(defn crossover [subs ind1 ind2]
  (let [children-per-fn (map crossover-single subs ind1 ind2)]
    (apply map vector children-per-fn)))

(defn mutate [task individual]
  (for [fx individual] 
    (for [cmd fx]
      (if (< (rand) 0.15)
        (rand-nth (:allowed-commands task))
        cmd))))

(defn fitness [task individual]
  (let [{:keys [status stats] :as res} (run-program individual task)
        score (if (= :success status)
                1e6
                (+ (* 10 (:stars-found stats))
                   (* 1 (:tiles-visited stats))))] 
    (assoc res :score score)))

(def score-comparator (comparator (fn [a b] (< (:score a) (:score b)))))

(defn run-generation [task population]
  (let [fitnesses (pmap (partial fitness task) population)
        sm (into (sorted-map-by score-comparator) (mapv vector fitnesses population))
        [best-fitness best-individual] (last sm)
        n (count population)
        good-third (mapv second (take (inc (int (/ n 3))) (reverse (seq sm))))
        children (mapcat (partial crossover (:subs task)) (shuffle good-third) (shuffle good-third))
;        children (concat good-third good-third)
        new-population (concat good-third (map (partial mutate task) children))
        perfect? (= 1e6 best-fitness)]
    (when perfect?
      (println "found one:" (pr-str best-individual) best-fitness))
    (with-meta new-population 
      {:best-fitness best-fitness
       :best-individual best-individual
       :perfect? perfect?})))

(defn take-until
  "Returns a lazy sequence of successive items from coll until
  (pred item) returns true, including that item. pred must be
  free of side-effects."
  [pred coll]
  (lazy-seq
    (when-let [s (seq coll)]
      (if (pred (first s))
        (cons (first s) nil)
        (cons (first s) (take-until pred (rest s)))))))

;;;;;;;;;;;;;;;; web interaction ;;;;;;;;;;;;;;;;;;;;
(defn- encode-program [program]
  (let [actions {:move \F :left \L :right \R :mark-red \r :mark-green \g :mark-blue \b :f1 1 :f2 2 :f3 3 :f4 4 :f5 5}
        colors {\b \b \g \g \r \r nil \_}
        enc (cs/join "|" (for [fn program] (apply str (for [[opcode color] fn] (str (colors color) (actions opcode))))))]
    (cs/join "|" (cons enc (repeat (- 5 (count program)) "")))))

(defn login [user pw]
  (let [my-cs (clj-http.cookies/cookie-store)
        login-success-page (:body (http/get "http://robozzle.com/login.aspx" {:cookie-store my-cs}))
        [_ validation] (re-find #"id=\"__EVENTVALIDATION\" value=\"(.*)\"" login-success-page)
        [_ view-state] (re-find #"id=\"__VIEWSTATE\" value=\"(.*)\"" login-success-page)]
    (println 
      (:status 
        (http/post "http://robozzle.com/login.aspx"
                   {:query-params {"ReturnURL" "/js/index.aspx"}
                    :cookie-store my-cs
                    :follow-redirects false
                    :headers {"Referer" "http://robozzle.com/login.aspx?ReturnURL=/user.aspx?name=cljgabot"}
                    :form-params {"__VIEWSTATE" view-state
                                  "__EVENTVALIDATION" validation
                                  "UserEmail" user
                                  "UserPass" pw
                                  "Submit1" "Log On"}})))
    my-cs))

(defn post-program [task program cookie-store]
  (http/post "http://robozzle.com/js/submit.aspx"
             {:headers {"Referer" (str "http://robozzle.com/play.aspx?puzzle=" (:id task))
                        "Origin" "http://robozzle.com"}
              :cookie-store cookie-store
              :follow-redirects false
              :form-params {"levelId" (:id task)
                            "solution" (encode-program program)}}))

(comment
  (def task (parse "d:\\Dropbox\\workspaces\\private-workspace\\robozzleGA\\play.aspx@puzzle=27"))
  (def task (parse (rand-nth (file-seq (as-file "/home/steffen/daten/privat/robozzle/")))))
  (print-state (init-state task []))
  (run-program [[[:move] [:right \b] [:right \g] [:f1]]] task)
  (run-program [[[:move nil] [:f1 \b] [:right \r] [:right \r] [:move] [:f1 nil]]] task)
  
  (require '[incanter.core :refer [view]])
  (require '[incanter.charts :as ic])
  (time 
    (let [population (repeatedly 300 #(generate-individual task))
          generations (iterate (partial run-generation task) population)
          results (take 1000 (take-until #(-> % meta :perfect?) generations))
          best (-> results last meta)]
      (view (ic/xy-plot (range) (map (comp :score :best-fitness meta) results)))
      (clojure.pprint/pprint best)
      (run-program (:best-individual best) task)))
  )