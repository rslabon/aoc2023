package aoc2023;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;

public class Day23 {

    static List<Point> adj(Point p, Grid grid) {
        return p.adj()
                .stream()
                .filter(n -> grid.inBoundry(n) && grid.isAllowed(n, Set.of("#")))
                .filter(n -> {
                    Direction nextDir = p.directionTo(n);
                    String nextCell = grid.get(n.x(), n.y());
                    if (nextDir == Direction.LEFT && nextCell.equals(">")) {
                        return false;
                    }
                    if (nextDir == Direction.RIGHT && nextCell.equals("<")) {
                        return false;
                    }
                    if (nextDir == Direction.UP && nextCell.equals("v")) {
                        return false;
                    }
                    if (nextDir == Direction.DOWN && nextCell.equals("^")) {
                        return false;
                    }
                    return true;
                })
                .collect(Collectors.toList());
    }

    static List<Point> adj2(Point p, Grid grid) {
        return p.adj()
                .stream()
                .filter(n -> grid.inBoundry(n) && grid.isAllowed(n, Set.of("#")))
                .collect(Collectors.toList());
    }

    static int max = 0;

    static void backtracking(Point current, Point target,
                             Graph<Point> graph,
                             Set<Point> visited,
                             int size) {
        if (current.equals(target)) {
            max = Math.max(max, size);
            return;
        }
        Set<Point> tmpVisited = new HashSet<>(visited);
        tmpVisited.add(current);
        for (Point next : graph.adj(current)) {
            if (!tmpVisited.contains(next)) {
                backtracking(next, target, graph, tmpVisited, size + graph.dist(current, next));
            }
        }
    }

    static void backtracking(Point current, Point target,
                             Function<Point, Collection<Point>> adj,
                             Set<Point> visited,
                             int size) {
        if (current.equals(target)) {
            max = Math.max(max, size);
            return;
        }
        Set<Point> tmpVisited = new HashSet<>(visited);
        tmpVisited.add(current);
        for (Point next : adj.apply(current)) {
            if (!tmpVisited.contains(next)) {
                backtracking(next, target, adj, tmpVisited, size + 1);
            }
        }
    }

    public static void main(String[] args) throws Exception {
//        String input = example;
        String input = Files.readString(Path.of("resources/day23.txt"));
        Grid grid = new Grid(input.split("\n"));

        int part1 = getPart1(grid);
        System.err.println("part1=" + part1);//2230

        int part2 = getPart2(grid);//compress grid into crossroads and compute distances between them
        System.err.println("part2=" + part2);//6542
    }

    private static int getPart1(Grid grid) {
        max = 0;
        Point start = new Point(0, 1);
        Point end = new Point(grid.height() - 1, grid.width() - 2);
        backtracking(start, end, p -> adj(p, grid), new HashSet<>(), 0);
        return max;
    }

    private static int getPart2(Grid grid) {
        return longestPath(grid, p -> adj2(p, grid));
    }

    private static int longestPath(Grid grid, Function<Point, Collection<Point>> adj) {
        Point start = new Point(0, 1);
        Point end = new Point(grid.height() - 1, grid.width() - 2);

        Set<Point> crossroads = new HashSet<>();
        crossroads.add(start);
        for (int i = 0; i < grid.height(); i++) {
            for (int j = 0; j < grid.width(); j++) {
                if (!grid.get(i, j).equals("#")) {
                    Point point = new Point(i, j);
                    if (adj.apply(point).size() >= 3) {
                        crossroads.add(point);
                    }
                }
            }
        }
        crossroads.add(end);
        Graph<Point> graph = new Graph<>();
        for (Point crossroad : crossroads) {
            Queue<Point> q = new LinkedList<>();
            q.add(crossroad);
            Set<Point> visited = new HashSet<>();
            Map<Point, Integer> dist = new HashMap<>();
            dist.put(crossroad, 0);
            while (!q.isEmpty()) {
                Point current = q.poll();
                visited.add(current);
                Collection<Point> nextPoints = adj.apply(current);
                for (Point next : nextPoints) {
                    if (visited.contains(next)) {
                        continue;
                    } else if (crossroads.contains(next)) {
                        graph.addEdge(crossroad, next, dist.get(current) + 1);
                        graph.addEdge(next, crossroad, dist.get(current) + 1);
                    } else {
                        q.add(next);
                        dist.put(next, dist.get(current) + 1);
                    }
                }
            }
        }
        max = 0;
        backtracking(start, end, graph, new HashSet<>(), 0);
        return max;
    }

    static String example = "#.#####################\n" +
            "#.......#########...###\n" +
            "#######.#########.#.###\n" +
            "###.....#.>.>.###.#.###\n" +
            "###v#####.#v#.###.#.###\n" +
            "###.>...#.#.#.....#...#\n" +
            "###v###.#.#.#########.#\n" +
            "###...#.#.#.......#...#\n" +
            "#####.#.#.#######.#.###\n" +
            "#.....#.#.#.......#...#\n" +
            "#.#####.#.#.#########v#\n" +
            "#.#...#...#...###...>.#\n" +
            "#.#.#v#######v###.###v#\n" +
            "#...#.>.#...>.>.#.###.#\n" +
            "#####v#.#.###v#.#.###.#\n" +
            "#.....#...#...#.#.#...#\n" +
            "#.#########.###.#.#.###\n" +
            "#...###...#...#...#.###\n" +
            "###.###.#.###v#####v###\n" +
            "#...#...#.#.>.>.#.>.###\n" +
            "#.###.###.#.###.#.#v###\n" +
            "#.....###...###...#...#\n" +
            "#####################.#";
}
