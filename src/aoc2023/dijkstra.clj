(ns aoc2023.dijkstra)

(defn- infinite-distance [vertices]
  (into {} (map (fn [i] [i Double/POSITIVE_INFINITY]) vertices))
  )

(defn- compute-dist-and-prev-of
  [vertex dist prev neighbors-of edge-cost Q]
  (loop [neighbors (filter #(contains? (set Q) %) (neighbors-of vertex))
         dist dist
         prev prev]
    (if (empty? neighbors)
      [dist prev]
      (let [neighbor (first neighbors)
            alt (+ (get dist vertex) (edge-cost vertex neighbor))]
        (if (< alt (get dist neighbor))
          (recur (rest neighbors) (assoc dist neighbor alt) (assoc prev neighbor vertex))
          (recur (rest neighbors) dist prev)
          )
        )
      )
    ))

;1  function Dijkstra(Graph, source):
;2
;3      for each vertex v in Graph.Vertices:
;4          dist[v] ← INFINITY
;5          prev[v] ← UNDEFINED
;6          add v to Q
;7      dist[source] ← 0
;8
;9      while Q is not empty:
;10          u ← vertex in Q with min dist[u]
;11          remove u from Q
;12
;13          for each neighbor v of u still in Q:
;14              alt ← dist[u] + Graph.Edges(u, v)
;15              if alt < dist[v]:
;16                  dist[v] ← alt
;17                  prev[v] ← u
;18
;19      return dist[], prev[]
(defn dijkstra
  ([source vertices neighbors-of edge-cost]
   (let [dist (assoc (infinite-distance vertices) source 0)
         prev {}
         Q (sort-by #(get dist %) vertices)]
     (dijkstra source vertices neighbors-of edge-cost dist prev Q)))
  ([source vertices neighbors-of edge-cost dist prev]
   (let [Q (sort-by #(get dist %) vertices)]
     (dijkstra source vertices neighbors-of edge-cost dist prev Q))
   )
  ([source vertices neighbors-of edge-cost dist prev Q]
   (if (empty? Q)
     [dist prev]
     (let [u (first Q)
           [dist prev] (compute-dist-and-prev-of u dist prev neighbors-of edge-cost Q)
           Q (time (sort-by #(get dist %) (rest Q)))]
       (recur source vertices neighbors-of edge-cost dist prev Q)
       )
     )
   )
  )

(defn make-path
  [target prev]
  (loop [v target
         path []]
    (if (nil? v)
      (reverse path)
      (let [parent (get prev v)]
        (recur parent (conj path v))
        )
      )
    ))

(defn path-to
  [source target vertices neighbors-of edge-cost]
  (let [[_ prev] (dijkstra source vertices neighbors-of edge-cost)]
    (make-path target prev)
    )
  )