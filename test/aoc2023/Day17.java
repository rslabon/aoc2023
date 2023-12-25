package aoc2023;


import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;


record Move(Position p, Direction d, int steps) {
    Set<Move> next(int minSteps, int maxSteps) {
        if (steps > maxSteps) {
            return Collections.emptySet();
        }
        Set<Move> moves = new HashSet<>();
        if (steps < maxSteps) {
            moves.add(go(d));
        }
        if (steps >= minSteps) {
            for (Direction t : d.turn()) {
                moves.add(go(t));
            }
        }
        return moves;
    }

    public Move go(Direction d) {
        return new Move(p.go(d), d, this.d == d ? this.steps + 1 : 1);
    }
}

record Position(int x, int y) {
    Position go(Direction d) {
        return switch (d) {
            case LEFT -> new Position(x, y - 1);
            case RIGHT -> new Position(x, y + 1);
            case UP -> new Position(x - 1, y);
            case DOWN -> new Position(x + 1, y);
        };
    }
}

class CityBlock {
    int heat;
    Position p;
    Set<CityBlock> adj;

    public CityBlock(int heat, Position p) {
        this.heat = heat;
        this.p = p;
        this.adj = new HashSet<>();
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        CityBlock cityBlock = (CityBlock) o;
        return heat == cityBlock.heat && Objects.equals(p, cityBlock.p);
    }

    @Override
    public int hashCode() {
        return Objects.hash(heat, p);
    }

    @Override
    public String toString() {
        return "CityBlock{" +
                "heat=" + heat +
                ", p=" + p +
                '}';
    }
}

class HeatMap {
    private final Map<Position, CityBlock> cityBlocks = new HashMap<>();
    private Map<Move, Integer> dist = new HashMap<>();
    private Map<Move, Move> prev = new HashMap<>();
    int width = 0;
    int height = 0;

    public void newCityBlock(int heat, int x, int y) {
        CityBlock cityBlock = new CityBlock(heat, new Position(x, y));
        cityBlocks.put(cityBlock.p, cityBlock);
        width = Math.max(width, y + 1);
        height = Math.max(height, x + 1);
    }

    public CityBlock getCityBlock(int x, int y) {
        return cityBlocks.getOrDefault(new Position(x, y), null);
    }

    public boolean inBounds(Position p) {
        return p.x() >= 0 && p.x() < height && p.y() >= 0 && p.y() < width;
    }


    private int manhattanDistance(Position p1, Position p2) {
        return Math.abs(p1.x() - p2.x()) + Math.abs(p1.y() + p2.y());
    }

    public int astar(Position start, Position end, int minSteps, int maxSteps) {
        dist = new HashMap<>();
        prev = new HashMap<>();
        PriorityQueue<Move> q = new PriorityQueue<>((m1, m2) -> manhattanDistance(m1.p(), end) - manhattanDistance(m2.p(), end));
        //from top left corner we can go right or down
        Move m1 = new Move(start, Direction.RIGHT, 1);
        Move m2 = new Move(start, Direction.DOWN, 1);
        dist.put(m1, 0);
        dist.put(m2, 0);
        q.add(m1);
        q.add(m2);
        while (!q.isEmpty()) {
            Move current = q.poll();
            for (Move next : current.next(minSteps, maxSteps)) {
                if (next.steps() > maxSteps) {
                    continue;
                }
                Position nextPosition = next.p();
                if (!inBounds(nextPosition)) { //out of grid
                    continue;
                }
                int alt = dist.getOrDefault(current, Integer.MAX_VALUE) + getCityBlock(nextPosition.x(), nextPosition.y()).heat;
                if (alt < dist.getOrDefault(next, Integer.MAX_VALUE)) {
                    if (next.p().equals(end) && next.steps() < minSteps) {
                        //check only moves with min steps
                        continue;
                    }
                    dist.put(next, alt);
                    prev.put(next, current);
                    q.remove(next);
                    q.add(next);
                } else if (next.steps() >= minSteps) {
                    q.add(next);
                }
            }
        }
        List<Move> endMoves = dist.keySet().stream().filter(m -> m.p().equals(end)).toList();
        int minHeat = Integer.MAX_VALUE;
        for (Move endMove : endMoves) {
            int heat = dist.get(endMove);
            minHeat = Math.min(minHeat, heat);
//            printPath(endMove);
        }
        return minHeat;
    }

    public int part1(Position start, Position end) {
        return astar(start, end, 1, 3);
    }

    public int part2(Position start, Position end) {
        return astar(start, end, 4, 10);
    }

    public void printPath(Move move) {
        Map<Position, Move> path = new HashMap<>();
        while (move != null) {
            path.put(move.p(), move);
            move = prev.get(move);
        }

        for (int i = 0; i < height; i++) {
            for (int j = 0; j < width; j++) {
                CityBlock cityBlock = getCityBlock(i, j);
                if (path.containsKey(cityBlock.p)) {
                    Move m = path.get(cityBlock.p);
                    System.err.print(switch (m.d()) {
                        case RIGHT -> ">";
                        case LEFT -> "<";
                        case UP -> "^";
                        case DOWN -> "v";
                    });
                } else {
                    System.err.print(cityBlock.heat);
                }
            }
            System.err.println();
        }
    }
}

public class Day17 {

    static HeatMap parse(String input) {
        String[] lines = input.split("\n");
        HeatMap heatMap = new HeatMap();
        for (int i = 0; i < lines.length; i++) {
            char[] cityBlocks = lines[i].toCharArray();
            for (int j = 0; j < cityBlocks.length; j++) {
                int heat = Integer.parseInt("" + cityBlocks[j]);
                heatMap.newCityBlock(heat, i, j);
            }
        }
        return heatMap;
    }

    public static void main(String[] args) throws Exception {
//        HeatMap hm = parse(example2);
        HeatMap hm = parse(Files.readString(Path.of("resources/day17.txt")));
        int part1 = hm.part1(new Position(0, 0), new Position(hm.height - 1, hm.width - 1));
        System.err.println("part1 = " + part1);

        int part2 = hm.part2(new Position(0, 0), new Position(hm.height - 1, hm.width - 1));
        System.err.println("part2 = " + part2);
    }

    static String example = "2413432311323\n" +
            "3215453535623\n" +
            "3255245654254\n" +
            "3446585845452\n" +
            "4546657867536\n" +
            "1438598798454\n" +
            "4457876987766\n" +
            "3637877979653\n" +
            "4654967986887\n" +
            "4564679986453\n" +
            "1224686865563\n" +
            "2546548887735\n" +
            "4322674655533";

    static String example2 = "111111111111\n" +
            "999999999991\n" +
            "999999999991\n" +
            "999999999991\n" +
            "999999999991";
}
