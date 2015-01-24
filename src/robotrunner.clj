(ns robotrunner
  (:require [clojure.string :as cs]))

;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def directions {0 [ 0  1]
                 1 [ 1  0]
                 2 [ 0 -1]
                 3 [-1  0]})
(def colors [\r \g \b])
(def opcodes [:move :left :right :mark-red :mark-green :mark-blue :f1 :f2 :f3 :f4 :f5])
(def board-size [16 12])

(defrecord state [dir pos board ip stack])

;;;;;;;;;;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn tile-color [^Character tile]
  (java.lang.Character/toLowerCase tile))

(def sub2idx {:f1 0 :f2 1 :f3 2 :f4 3 :f5 4})
(def idx2sub [:f1 :f2 :f3 :f4 :f5])

(defn star? [tile]
  (java.lang.Character/isUpperCase tile))

(defn count-stars [board]
  (count (filter star? (apply concat board))))
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
  (let [colors (->> b (apply concat) (map tile-color) distinct)
        commands (list* (when (not= 0 (bit-and cmds 1)):mark-red)
                        (when (not= 0 (bit-and cmds 2)) :mark-green)
                        (when (not= 0 (bit-and cmds 4)) :mark-blue)
                        :move :left :right
                        (keep-indexed (fn [i v] (when (> v 0) (idx2sub i))) subs))]
    (for [cmd (remove nil? commands), color [nil \b \g \r]]
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
        board (parse-board (:board parsed))]
    {:allowed-commands (available-commands parsed)
     :pos [(:robotRow parsed) (:robotCol parsed)]
     :dir (:robotDir parsed)
     :board board
     :title (:title parsed)
     :id (:id parsed)
     :subs (remove zero? (:subs parsed))
     :stars (count-stars board)
     :non-blank-tiles (count (remove #{\space} (apply concat board)))}))


;;;;;;;;;;; simulator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn turn-left [dir]
  (mod (+ 3 dir) 4))

(defn turn-right [dir]
  (mod (inc dir) 4))

(defn won? [state]
  (zero? (count-stars (:board state))))

(defn run-step [{:keys [dir pos ip stack program] :as state}]
  (let [tile-path (apply vector :board pos)
        tile (get-in state tile-path) _ (when (nil? tile) (println "ERROR!") (print-state state))
        tile (tile-color tile)
        state (assoc-in state tile-path tile) ; clear star on current tile
        state (update-in state [:ip 1] inc) ; move instruction pointer
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
        nil (assoc state :ip (first stack) :stack (rest stack)) ; we ran out of opcodes, try to pop from stack
        ;else a function call
        (assoc state :ip [(sub2idx op-code) 0] :stack (cons (:ip state) stack))))))


(defn init-state [{:keys [dir pos board]} program]
  (map->state {:dir dir 
               :pos pos 
               :board board 
               :ip [0 0] 
               :stack []
               :program program}))

(defn valid? [{:keys [dir pos ip program board]}] ;(println "valid?" pos ip program (get-in program ip) (get-in board pos))
  (and (<= 0 (first pos) (second board-size))
       (<= 0 (second pos) (first board-size))
       (not (nil? (get-in program ip)))
       (#{\b \g \r \B \G \R} (get-in board pos))))

(defn update-statistics [stats task state]
  (let [stars (:stars task)
        stars-left (count-stars (:board state))
        seen ((fnil conj #{}) (:seen stats) (:pos state))]
    {:seen seen
     :stars-found (/ (- stars stars-left) stars)
     :tiles-visited (/ (count seen) (:non-blank-tiles task))}))

(defn run-program [program task]
  (let [state (init-state task program)
        steps (iterate run-step state)]
    (loop [steps steps, n 0, stats {}] 
      (let [state (first steps)
            stats (update-statistics stats task state)]
        (cond 
          (> n 100) {:status :too-long :steps n :stats stats}
          (not (valid? state)) {:status :invalid-move :steps n :stats stats}
          (nil? (:ip state)) {:status :no-more-moves :steps n :stats stats}
          (won? state) {:status :success :steps n :stats stats}
          :else (recur (next steps) (inc n) stats))))))

;;;;;;;;;;;;;;;;;;; Genetic Algorithms based solver ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn generate-individual [{subs :subs ac :allowed-commands}]
  (vec (for [n subs :when (pos? n)]
        (vec (repeatedly (rand n) #(rand-nth ac))))))

(defn crossover-single [max-length ind1 ind2]
  (let [[left-1 right-1] (split-at (inc (rand (count ind1))) ind1)
        [left-2 right-2] (split-at (inc (rand (count ind2))) ind2)]
    [(vec (take max-length (vec (concat left-1 right-2))))
     (vec (take max-length (vec (concat left-2 right-1))))]))

(defn crossover [subs ind1 ind2]
  (mapv crossover-single subs ind1 ind2))

(defn mutate [task individual]
  (for [fx individual] 
    (for [cmd fx]
      (if (< (rand) 0.05)
        (rand-nth (:allowed-commands task))
        cmd))))

(defn fitness [task individual]
  (let [{:keys [status stats] :as res} (run-program individual task)
        score (if (= :success status)
                1e6
                (+ (* 100 (:stars-found stats))
                   (* 10 (:tiles-visited stats))))] 
    (assoc res :score score)))

(defn run-generation [task population]
  (let [fitnesses (pmap (partial fitness task) population)
        sm (into (sorted-map) (mapv vector (mapv :score fitnesses) population))
        best-fitness (first (last sm))
        n (count population)
        good-third (mapv second (take (inc (int (/ n 3))) (reverse (seq sm))))
        best-individual (first good-third)
        children (apply concat (mapv (partial crossover (:subs task)) (shuffle good-third) (shuffle good-third)))
        new-population (concat good-third (map (partial mutate task) children))]
    (with-meta new-population 
      {:best-fitness best-fitness
       :best-individual best-individual
       :perfect? (= 1e6 best-fitness)})))

(comment
  (def task (parse "/home/steffen/Dropbox/Workspaces/private-workspace/robozzleGA/html/play.aspx?puzzle=140"))
  (print-state (init-state task []))
  (run-program [[[:move] [:right \b] [:right \g] [:f1]]] task)
  (run-program [[[:move nil] [:f1 \b] [:right \r] [:right \r] [:move] [:f1 nil]]] task)
  
  (require '[incanter.core :refer [view]])
  (require '[incanter.charts :as ic])
  (time 
    (let [population (repeatedly 300 #(generate-individual task))
          results (take 200 (iterate (partial run-generation task) population))]
      (view (ic/xy-plot (range) (map (comp :best-fitness meta) results)))
      (clojure.pprint/pprint (-> results last meta))))
  )