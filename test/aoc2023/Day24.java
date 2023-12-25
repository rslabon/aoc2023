package aoc2023;

import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

record Point3D(long x, long y, long z) {
}

record Hailstone(Point3D p, Point3D v) {
}

public class Day24 {

    public static void main(String[] args) throws Exception {
//        String input = example;
//        int min = 7;
//        int max = 27;
        long min = 200000000000000L;
        long max = 400000000000000L;
        String input = Files.readString(Path.of("resources/day24.txt"));
        List<Hailstone> hailstones = parse(input);

        int inside = 0;
        for (int i = 0; i < hailstones.size(); i++) {
            for (int j = i + 1; j < hailstones.size(); j++) {
                Point3D p1 = hailstones.get(i).p();
                Point3D v1 = hailstones.get(i).v();
                Point3D p2 = hailstones.get(j).p();
                Point3D v2 = hailstones.get(j).v();

                if (insideTestArea(p1, v1, p2, v2, min, max)) {
                    inside++;
                }
            }
        }

        System.err.println("part1 = " + inside);//13910
    }

    private static List<Hailstone> parse(String input) {
        List<Hailstone> hailstones = new ArrayList<>();
        for (String line : input.split("\n")) {
            String[] parts = line.split(" @ ");
            String position = parts[0];
            String velocity = parts[1];
            List<Long> positions = Arrays.stream(position.trim().split(", ")).map(String::trim).map(Long::parseLong).toList();
            List<Long> velocities = Arrays.stream(velocity.trim().split(", ")).map(String::trim).map(Long::parseLong).toList();
            hailstones.add(new Hailstone(
                    new Point3D(positions.get(0), positions.get(1), positions.get(2)),
                    new Point3D(velocities.get(0), velocities.get(1), velocities.get(2))
            ));
        }
        return hailstones;
    }

    private static boolean insideTestArea(Point3D p1, Point3D v1, Point3D p2, Point3D v2, long min, long max) {
        long t1b = v1.x() * v2.y() - v1.y() * v2.x();
        long t2b = v1.y() * v2.x() - v1.x() * v2.y();
        if (t1b == 0 || t2b == 0) {
            return false;
        }
        double t1 = (p2.x() * v2.y() + p1.y() * v2.x() - p2.y() * v2.x() - p1.x() * v2.y()) / (double) t1b;
        double t2 = (p1.x() * v1.y() + p2.y() * v1.x() - p1.y() * v1.x() - p2.x() * v1.y()) / (double) t2b;
        if (t1 < 0 || t2 < 0) {
            return false;
        }

        double x = p1.x() + t1 * v1.x();
        double y = p1.y() + t1 * v1.y();

        return x >= min && x <= max && y >= min && y <= max;
    }

    static String example = "19, 13, 30 @ -2,  1, -2\n" +
            "18, 19, 22 @ -1, -1, -2\n" +
            "20, 25, 34 @ -2, -2, -4\n" +
            "12, 31, 28 @ -1, -2, -1\n" +
            "20, 19, 15 @  1, -5, -3";
}
