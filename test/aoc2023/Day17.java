package aoc2023;


import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.IntStream;

enum Direction {
    LEFT {
        @Override
        boolean isNegative(Direction other) {
            return other == RIGHT;
        }
    }, RIGHT {
        @Override
        boolean isNegative(Direction other) {
            return other == LEFT;
        }
    }, UP {
        @Override
        boolean isNegative(Direction other) {
            return other == DOWN;
        }
    }, DOWN {
        @Override
        boolean isNegative(Direction other) {
            return other == UP;
        }
    };

    abstract boolean isNegative(Direction other);
}

record Move(Position p, Direction d, int steps) {
    List<Move> next(int minSteps, int maxSteps) {
        return gen(minSteps)
                .stream()
                .filter(m -> !d.isNegative(m.d))
                .filter(m -> m.steps < maxSteps)
                .toList();
    }

    private Set<Move> gen(int minSteps) {
        Set<Move> result = new HashSet<>();
        result.addAll(IntStream.range(1, minSteps + 1).mapToObj(i -> go(Direction.LEFT, i)).collect(Collectors.toSet()));
        result.addAll(IntStream.range(1, minSteps + 1).mapToObj(i -> go(Direction.RIGHT, i)).collect(Collectors.toSet()));
        result.addAll(IntStream.range(1, minSteps + 1).mapToObj(i -> go(Direction.UP, i)).collect(Collectors.toSet()));
        result.addAll(IntStream.range(1, minSteps + 1).mapToObj(i -> go(Direction.DOWN, i)).collect(Collectors.toSet()));
        return result;
    }

    public Move go(Direction d, int steps) {
        return new Move(p.go(d, steps), d, this.d == d ? this.steps + steps : 1);
    }

//    private List<Move> singleNext() {
//        return Stream.of(
//                        new Move(p.go(Direction.RIGHT), Direction.RIGHT, d == Direction.RIGHT ? steps + 1 : 1),
//                        new Move(p.go(Direction.LEFT), Direction.LEFT, d == Direction.LEFT ? steps + 1 : 1),
//                        new Move(p.go(Direction.UP), Direction.UP, d == Direction.UP ? steps + 1 : 1),
//                        new Move(p.go(Direction.DOWN), Direction.DOWN, d == Direction.DOWN ? steps + 1 : 1))
//                .filter(m -> !d.isNegative(m.d)) //cannot reverse
//                .toList();
//    }
}

record Position(int x, int y) {
    Position go(Direction d, int steps) {
        return switch (d) {
            case LEFT -> new Position(x, y - steps);
            case RIGHT -> new Position(x, y + steps);
            case UP -> new Position(x - steps, y);
            case DOWN -> new Position(x + steps, y);
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

    public CityBlock newCityBlock(int heat, int x, int y) {
        CityBlock cityBlock = new CityBlock(heat, new Position(x, y));
        cityBlocks.put(cityBlock.p, cityBlock);
        width = Math.max(width, y + 1);
        height = Math.max(height, x + 1);
        return cityBlock;
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
            if (current.steps() > maxSteps || current.steps() < minSteps) {
                continue;
            }
            for (Move next : current.next(minSteps, maxSteps)) {
//                if ((current.d() != next.d() && next.steps() < minSteps) || next.steps() > maxSteps) {
//                    continue;
//                }
                Position nextPosition = next.p();
                if (!inBounds(nextPosition)) { //out of grid
                    continue;
                }
                int alt = dist.getOrDefault(current, Integer.MAX_VALUE) + getCityBlock(nextPosition.x(), nextPosition.y()).heat;
                if (alt < dist.getOrDefault(next, Integer.MAX_VALUE)) {
                    dist.put(next, alt);
                    prev.put(next, current);
                    q.remove(next);
                    q.add(next);
                }
            }
        }
        List<Move> endMoves = dist.keySet().stream().filter(m -> m.p().equals(end)).toList();
        int minHeat = Integer.MAX_VALUE;

        printPath();

        for (Move endMove : endMoves) {
            int heat = dist.get(endMove);
            minHeat = Math.min(minHeat, heat);
            printPath(endMove);
            System.err.println("\n\n");
        }
        return minHeat;
    }

    public int part1(Position start, Position end) {
        return astar(start, end, 1, 3);
    }

    public int part2(Position start, Position end) {
        return astar(start, end, 1, 10);
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

    public void printPath() {
        Map<Position, Move> path = new HashMap<>();
        for (Map.Entry<Move, Move> e : prev.entrySet()) {
            path.put(e.getKey().p(), e.getKey());
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
        HeatMap hm = parse(example);
//        HeatMap hm = parse(Files.readString(Path.of("resources/day17.txt")));
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
