package aoc2023;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

record Grid(String[] lines) {

    int width() {
        return lines[0].length();
    }

    int height() {
        return lines.length;
    }

    String get(int x, int y) {
        return "" + lines[x].toCharArray()[y];
    }

    Point find(String cell) {
        for (int i = 0; i < height(); i++) {
            for (int j = 0; j < width(); j++) {
                if (get(i, j).equals(cell)) {
                    return new Point(i, j);
                }
            }
        }
        throw new IllegalStateException("Missing!");
    }

    public boolean inBoundry(Point p) {
        return p.x() >= 0 && p.x() < height() && p.y() >= 0 && p.y() < width();
    }

    public boolean isAllowed(Point p, Set<String> prohibited) {
        return !prohibited.contains(get(p.x(), p.y()));
    }

    public void print(Collection<Point> points, String cell) {
        Set<Point> s = new HashSet<>(points);
        for (int i = 0; i < height(); i++) {
            for (int j = 0; j < width(); j++) {
                if (s.contains(new Point(i, j))) {
                    System.err.print(cell);
                } else {
                    System.err.print(get(i, j));
                }
            }
            System.err.println();
        }
    }
}