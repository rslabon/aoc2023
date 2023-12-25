package aoc2023;

import java.util.*;

record Edge<T>(T node, Integer distance) {
}

public class Graph<T> {
    private Map<T, Set<Edge<T>>> edges = new HashMap<>();

    void addEdge(T from, T to, Integer distance) {
        Set<Edge<T>> others = edges.getOrDefault(from, new HashSet<>());
        others.add(new Edge<>(to, distance));
        edges.put(from, others);
    }

    Collection<T> adj(T node) {
        return edges.getOrDefault(node, new HashSet<>())
                .stream()
                .map(Edge::node)
                .toList();
    }

    int dist(T from, T to) {
        return edges.getOrDefault(from, new HashSet<>())
                .stream()
                .filter(e -> e.node().equals(to))
                .map(Edge::distance)
                .findFirst()
                .orElseThrow();
    }
}
