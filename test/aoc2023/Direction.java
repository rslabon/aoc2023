package aoc2023;

import java.util.Set;

enum Direction {
    LEFT {
        @Override
        Set<Direction> turn() {
            return Set.of(UP, DOWN);
        }
    },
    RIGHT {
        @Override
        Set<Direction> turn() {
            return Set.of(UP, DOWN);
        }
    },
    UP {
        @Override
        Set<Direction> turn() {
            return Set.of(LEFT, RIGHT);
        }
    },
    DOWN {
        @Override
        Set<Direction> turn() {
            return Set.of(LEFT, RIGHT);
        }
    };

    abstract Set<Direction> turn();
}