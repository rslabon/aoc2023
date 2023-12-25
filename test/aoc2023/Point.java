package aoc2023;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

record Point(int x, int y) {
    List<Point> adj() {
        return List.of(
                new Point(x + 1, y),
                new Point(x - 1, y),
                new Point(x, y + 1),
                new Point(x, y - 1)
        );
    }


    Point adjust(int height, int width) {
        int nx = x;
        int ny = y;
        if (nx < 0) {
            nx = height - (Math.abs(nx) % height);
        }
        if (nx >= height) {
            nx = nx % height;
        }
        if (ny < 0) {
            ny = width - (Math.abs(ny) % width);
        }
        if (ny >= width) {
            ny = ny % width;
        }
        return new Point(nx, ny);
    }

    public Direction directionTo(Point other) {
        if (x > other.x) return Direction.UP;
        if (y > other.y) return Direction.LEFT;
        if (x < other.x) return Direction.DOWN;
        if (y < other.y) return Direction.RIGHT;
        throw new IllegalStateException("Unknown direction!");
    }
}