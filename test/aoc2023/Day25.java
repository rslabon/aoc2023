package aoc2023;

import guru.nidi.graphviz.attribute.*;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;

import java.io.File;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

import static guru.nidi.graphviz.attribute.Rank.RankDir.TOP_TO_BOTTOM;
import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;

class ConnectedComponents<T> {
    public int id = 0;

    private Graph<T> graph;
    public Map<T, Integer> components = new HashMap<>();

    public ConnectedComponents(Graph<T> graph) {
        this.graph = graph;
    }

    void dfs(T node, Set<T> visited) {
        if (visited.contains(node)) {
            return;
        }
        visited.add(node);
        components.put(node, id);
        for (T other : graph.adj(node)) {
            if (!visited.contains(other)) {
                dfs(other, visited);
            }
        }
    }

    void compute() {
        Set<T> visited = new HashSet<>();
        for (T v : graph.getVertices()) {
            if (!visited.contains(v)) {
                id++;
                dfs(v, visited);
            }
        }
    }
}


public class Day25 {


    public static void main(String[] args) throws Exception {
//        String input = example;

        String input = Files.readString(Path.of("resources/day25.txt"));
        Graph<String> graph = parse(input);

        List<UndirectionalEdge<String>> edges = graph.edges().stream()
                .map(p -> new UndirectionalEdge<>(p.first(), p.second()))
                .collect(Collectors.toSet())
                .stream().toList();


//        saveGraphAsSVG(input, "graph.svg");//manually deleted nodes in svg!!!
        graph.remove("kkp", "vtv");
        graph.remove("vtv", "kkp");
        graph.remove("cmj", "qhd");
        graph.remove("qhd", "cmj");

        for (int i = 0; i < edges.size(); i++) {
            UndirectionalEdge<String> e1 = edges.get(i);
            Graph<String> copy = graph.copy();

            copy.remove(e1.v1, e1.v2);
            copy.remove(e1.v2, e1.v1);
            ConnectedComponents<String> cc = new ConnectedComponents<>(copy);
            cc.compute();
            if (cc.id >= 2) {
                Map<Integer, List<Integer>> byId = cc.components.values().stream()
                        .collect(Collectors.groupingBy(Function.identity()));
                int power = byId.values().stream().map(List::size).reduce((acc, s) -> acc * s)
                        .orElseThrow();

                System.err.println(e1);
                System.err.println("power: " + power);
            }
        }
    }

    private static Graph<String> parse(String input) {
        Graph<String> graph = new Graph<>();
        for (String line : input.split("\n")) {
            String[] parts = line.split(": ");
            String from = parts[0];
            List<String> adj = Arrays.stream(parts[1].split(" ")).toList();
            for (String other : adj) {
                graph.addEdge(from, other, 0);
                graph.addEdge(other, from, 0);
            }
        }
        return graph;
    }

    private static void saveGraphAsSVG(String input, String name) throws Exception {
        guru.nidi.graphviz.model.Graph g = graph("")
                .nodeAttr().with(Font.name("arial"))
                .graphAttr().with(Rank.dir(TOP_TO_BOTTOM))
                .graphAttr().with(Attributes.attr("LANDSCAPE", true))
                .linkAttr().with("class", "link-class");
        for (String line : input.split("\n")) {
            String[] parts = line.split(": ");
            String from = parts[0];
            List<String> adj = Arrays.stream(parts[1].split(" ")).toList();
            for (String other : adj) {
                g = g.with(node(from).link(node(other)).with(Style.BOLD, Label.of("100 times"), Color.RED));
            }
        }
        Graphviz.fromGraph(g)
                .totalMemory(1000000000)
                .width(1024 * 50)
                .height(968 * 50)
                .render(Format.SVG).toFile(new File(name));
    }

    static String example = "jqt: rhn xhk nvd\n" +
            "rsh: frs pzl lsr\n" +
            "xhk: hfx\n" +
            "cmg: qnr nvd lhk bvb\n" +
            "rhn: xhk bvb hfx\n" +
            "bvb: xhk hfx\n" +
            "pzl: lsr hfx nvd\n" +
            "qnr: nvd\n" +
            "ntq: jqt hfx bvb xhk\n" +
            "nvd: lhk\n" +
            "lsr: lhk\n" +
            "rzs: qnr cmg lsr rsh\n" +
            "frs: qnr lhk lsr";

    static class UndirectionalEdge<T> {
        public T v1;
        public T v2;

        public UndirectionalEdge(T v1, T v2) {
            this.v1 = v1;
            this.v2 = v2;
        }

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            UndirectionalEdge<?> that = (UndirectionalEdge<?>) o;
            return (Objects.equals(v1, that.v1) && Objects.equals(v2, that.v2))
                    || (Objects.equals(v1, that.v2) && Objects.equals(v2, that.v1));
        }

        @Override
        public int hashCode() {
            return Objects.hash(v1) * Objects.hash(v2);
        }

        @Override
        public String toString() {
            return v1 + " ---> |" + v1 + "-" + v2 + "| " + v2;
        }
    }
}
