package aoc2023;

import java.util.*;
import java.util.stream.Collectors;

record Edge<T>(T node, Integer distance) {
}

public class Graph<T> {
    private Map<T, Set<Edge<T>>> edges = new HashMap<>();
    private Set<T> vertices = new HashSet<>();

    void addEdge(T from, T to, Integer distance) {
        vertices.add(from);
        vertices.add(to);
        Set<Edge<T>> others = edges.getOrDefault(from, new HashSet<>());
        others.add(new Edge<>(to, distance));
        edges.put(from, others);
    }

    public Set<T> getVertices() {
        return Set.copyOf(vertices);
    }

    Collection<T> adj(T node) {
        return edges.getOrDefault(node, new HashSet<>())
                .stream()
                .map(Edge::node)
                .toList();
    }

    List<Pair<T>> edges() {
        List<Pair<T>> result = new ArrayList<>();
        for (Map.Entry<T, Set<Edge<T>>> entry : edges.entrySet()) {
            for (Edge<T> edge : entry.getValue()) {
                result.add(new Pair<>(entry.getKey(), edge.node()));
            }
        }
        return result;
    }

    int dist(T from, T to) {
        return edges.getOrDefault(from, new HashSet<>())
                .stream()
                .filter(e -> e.node().equals(to))
                .map(Edge::distance)
                .findFirst()
                .orElseThrow();
    }

    Graph<T> copy() {
        Graph<T> copy = new Graph<>();
        copy.vertices = new HashSet<>(this.vertices);
        copy.edges = new HashMap<>();
        for (Map.Entry<T, Set<Edge<T>>> entry : this.edges.entrySet()) {
            copy.edges.put(entry.getKey(), new HashSet<>(entry.getValue()));
        }
        return copy;
    }

    void remove(T from, T to) {
        Set<Edge<T>> adj = edges.getOrDefault(from, new HashSet<>());
        adj = adj.stream().filter(e -> !e.node().equals(to)).collect(Collectors.toSet());
        edges.put(from, adj);
    }
}
