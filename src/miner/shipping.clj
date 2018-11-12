(ns miner.shipping
  (:require [clojure.string :as str]))

;; Original source: https://kevinlynagh.com/notes/shipping-puzzle/
;; "Exploring a shipping puzzle" by Kevin Lynagh.  @lynaghk
;; Adapted by Steve Miner @miner.

(def simple
  [{:id 1, :dow "M", :start "PDX", :end "SEA"}
   {:id 2, :dow "T", :start "PDX", :end "SFO"}
   {:id 3, :dow "T", :start "SEA", :end "DEN"}
   {:id 4, :dow "W", :start "DEN", :end "PDX"}
   {:id 5, :dow "R", :start "PDX", :end "DEN"}
   {:id 6, :dow "F", :start "DEN", :end "JFK"}])

;; SEM: I didn't see any need to make legs into a set.  Each leg already has a unique ID so
;; there's no chance of duplicates.

(def legs
  (->> (str/split (slurp "resources/legs.txt") #"\n")
       (map (fn [s]
              (let [[id start end dow] (str/split s #"\s+")]
                {:id id :start start :end end :dow dow})))
      ))

(def tomorrow
  {"M" "T", "T" "W", "W" "R", "R" "F"})

;;; SEM: use `peek` instead of `last` on the vector leg.  It's much faster.

(defn add-legs
  [dow possible-legs routes]
  (reduce (fn [routes {:keys [start] :as l}]
            (if-let [r (->> routes
                            (filter #(let [last-leg (peek %)]
                                       (and (= dow (tomorrow (:dow last-leg)))
                                            (= start (:end last-leg)))))
                            first)]
              (-> routes
                  (disj r)
                  (conj (conj r l)))
              (conj routes [l])))
          routes possible-legs))

(defn attempt-1
  [legs]
  (let [dow-legs (group-by :dow legs)]
    (->> #{}
         (add-legs "M" (dow-legs "M"))
         (add-legs "T" (dow-legs "T"))
         (add-legs "W" (dow-legs "W"))
         (add-legs "R" (dow-legs "R"))
         (add-legs "F" (dow-legs "F")))))


(defn attempt-2
  [legs]
  (let [dow-legs (group-by :dow legs)]
    (loop [[dow & dows] ["M" "T" "W" "R" "F"]
           routes-by-position {}
           finished-routes #{}]

      (if (nil? dow)
        ;;done
        (reduce into finished-routes (vals routes-by-position))

        ;;else get all of today's legs and try to attach to routes
        (let [[extended-routes-by-position unextended-routes-by-position]
              (loop [[l & ls] (dow-legs dow)
                     extended-routes-by-position {}
                     unextended-routes-by-position routes-by-position]
                (if (nil? l)
                  [extended-routes-by-position unextended-routes-by-position]
                  (if-let [r (-> unextended-routes-by-position
                                 (get (:start l))
                                 first)]

                    ;;Add this leg to a route
                    (recur ls
                           (update extended-routes-by-position (:end l) conj (conj r l))
                           (update unextended-routes-by-position (:start l) rest))

                    ;;Leg can't be added to existing route, start a new one
                    (recur ls
                           (update extended-routes-by-position (:end l) conj [l])
                           unextended-routes-by-position))))]

          (recur dows
                 ;;extended routes can keep being extended
                 extended-routes-by-position
                 ;;routes that weren't extended today must be finished; move them out so they won't slow down future calcs
                 (reduce into finished-routes (vals unextended-routes-by-position))))))))




;;; ----------------------------------------------------------------------
;;; Steve's solution
;;;
;;; Keep legs partitioned by :dow and sorted by :start.  Keep paths partitioned by last :dow
;;; and sorted by the last :end.  That simplifies connecting the new leg to an existing
;;; path.  In my tests, it's slightly faster.

(defn by-end [path]
  (:end (peek path)))

(defn extend-paths [[old-paths today-paths] day-legs]
  ;; today-paths are sorted by :end (and exclusively "today" :dow)
  ;; day-legs are just next day, sorted by :start
  ;; old-paths are ones that couldn't be extended
  ;; new-paths are extended from today-paths with one of day-legs
  ;;
  ;; returns vector of [old-paths tomorrow-paths]
  ;; with tomorrow-paths sorted by :end
  (loop [legs day-legs paths today-paths tomorrow-paths [] olds old-paths]
    (let [leg (first legs)
          p (first paths)]
      (if (nil? leg)
        [(into olds paths) (sort-by by-end tomorrow-paths)]
        (if (nil? p)
          [olds (sort-by by-end (into tomorrow-paths (map vector legs)))]
          (let [end-cmp (compare (:start leg) (:end (peek p)))]
            (if (zero? end-cmp)
              (recur (rest legs) (rest paths) (conj tomorrow-paths (conj p leg)) olds)
              (if (pos? end-cmp)
                (recur legs (rest paths) tomorrow-paths (conj olds p))
                (recur (rest legs) paths (conj tomorrow-paths (vector leg)) olds)))))))))

;; Legs per day are sorted by :start.  Existings paths sorted by last :end.
(defn shipping [legs]
  (let [day-legs (group-by :dow legs)]
    (apply concat
           (reduce extend-paths
                   [[] (mapv vector (sort-by :end (day-legs "M")))]
                   (map #(sort-by :start %) (map day-legs ["T" "W" "R" "F"]))))))



(defn smoke-test [f]
  (assert (= (time (count (f legs))) 2414))
  true)
