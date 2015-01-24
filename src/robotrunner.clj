(ns robotrunner
  (:require [clojure.string :as cs]))

;;;;;;;;; constants ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def directions {0 [ 0  1]
                 1 [ 1  0]
                 2 [ 0 -1]
                 3 [-1  0]})
(def colors [\b \g \r])
(def opcodes [:move :left :right :mark-red :mark-green :mark-blue :f1 :f2 :f3 :f4 :f5])
(def board-size [16 12])

(defrecord state [dir pos board ip stars-left stack])

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
  (clojure.pprint/pprint (assoc state :board (board2str state))))

(defn parse [file]
  (let [token (->> file
                slurp
                (re-find #"(?s)var puzzles = \[\{(.*)\}\]")
                second
                cs/trim
                cs/split-lines)
        pairs (map #(map cs/trim (cs/split % #":")) token)
        parsed-pairs (map (fn [[k v]] [(keyword k) (read-string v)]) pairs)
        m (into {} parsed-pairs)]
    (update m :board parse-board)))

;;;;;;;;;;;; helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn star? [tile]
  (java.lang.Character/isUpperCase tile))

(defn tile-color [^Character tile]
  (java.lang.Character/toLowerCase tile))

(defn turn-left [dir]
  (mod (+ 3 dir) 4))

(defn turn-right [dir]
  (mod (inc dir) 4))

(defn won? [state]
  (zero? (:stars-left state)))

(defn valid? [{[x y] :pos board :board pos :pos ip :ip} program]
  (and (<= 0 x (second board-size))
       (<= 0 y (first board-size))
       (not (nil? (get-in program ip)))
       (#{\b \g \r \B \G \R} (get-in board pos))))

;;;;;;;;;;; simulator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn run-step [program {:keys [dir pos ip stack] :as state}]
  (let [tile-path (apply vector :board pos)
        tile (get-in state tile-path)
        star? (star? tile)
        tile (tile-color tile)
        state (if star? (update state :stars-left dec) state) ;decrease number of stars left
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
        :mark-blue (assoc-in state tile-path \b)
        :mark-green (assoc-in state tile-path \g)
        nil (assoc state :ip (first stack) :stack (rest stack)) ; we ran out of opcodes, try to pop from stack
        ;else a function call
        (assoc state :ip [({:f1 0 :f2 1 :f3 2 :f4 3 :f5 4} op-code) 0] :stack (cons (:ip state) stack))))))

(defn init-state [{:keys [robotRow robotCol robotDir board]}]
  (map->state {:dir robotDir 
               :pos [robotRow robotCol] 
               :board board 
               :ip [0 0] 
               :stars-left (count (filter star? (apply concat board)))
               :stack []}))

(defn run-program [program task]
  (let [state (init-state task)
        steps (iterate (partial run-step program) state)]
    (loop [[state & steps] steps, n 0] ;(println n) (print-state state)
      (cond 
        (> n 1000) [:too-long n]
        (not (valid? state program)) [:invalid-move n]
        (nil? (:ip state)) [:no-more-moves n]
        (won? state) [:success n]
        :else (recur steps (inc n))))))

(comment
  (def task (parse "/home/steffen/Dropbox/Workspaces/private-workspace/robozzleGA/html/play.aspx?puzzle=140"))
  (print-state (init-state task))
  (run-program [[[:move] [:right \b] [:right \g] [:f1]]] task)
  (run-program [[[:move nil] [:f1 \b] [:right \r] [:right \r] [:move] [:f1 nil]]] task)
  )