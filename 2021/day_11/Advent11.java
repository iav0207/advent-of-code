import java.io.*;
import java.util.stream.*;
import java.util.*;

public class Advent11 {
    public static void main(String... args) {
        var field = new Field(new BufferedReader(new InputStreamReader(System.in)));

        var step = 0;
        var flashes = 0;
        Optional<Integer> p1 = Optional.empty();
        Optional<Integer> p2 = Optional.empty();

        while (p1.isEmpty() || p2.isEmpty()) {
            step += 1;
            var todo = new ArrayDeque<Coord>();
            var flashed = new HashSet<Coord>();

            for (int i = 0; i < field.n; i++) {
                for (int j = 0; j < field.m; j++) {
                    if (field.inc(i, j) > 9) {
                        todo.add(new Coord(i, j));
                    }
                }
            }

            while (!todo.isEmpty()) {
                var flash = todo.poll();
                if (!flashed.add(flash))
                    continue;
                field.adj(flash)
                        .filter(it -> field.inc(it.i, it.j) > 9)
                        .forEach(todo::add);
            }

            flashed.forEach(field::reset);
            flashes += flashed.size();

            if (step == 100)
                p1 = Optional.of(flashes);
            if (p2.isEmpty() && flashed.size() == field.n * field.m)
                p2 = Optional.of(step);
        }

        p1.ifPresent(v -> System.out.printf("Part 1: %d\n", v));
        p2.ifPresent(v -> System.out.printf("Part 2: %d\n", v));
    }

    static class Field {
        final List<List<Integer>> f;
        final int n, m;

        Field(BufferedReader r) {
            f = r.lines()
                    .filter(line -> !line.isBlank())
                    .map(line -> Arrays.stream(line.split(""))
                            .map(Integer::parseInt)
                            .collect(Collectors.toList()))
                    .collect(Collectors.toList());
            n = f.size();
            m = f.get(0).size();
        }

        int inc(int i, int j) {
            var row = f.get(i);
            row.set(j, row.get(j) + 1);
            return row.get(j);
        }

        int get(int i, int j) {
            return f.get(i).get(j);
        }

        void set(int i, int j, int val) {
            f.get(i).set(j, val);
        }

        Stream<Coord> adj(Coord c) {
            return Stream.of(-1, 0, 1)
                    .flatMap(di -> Stream.of(-1, 0, 1).map(dj -> new Coord(di, dj)))
                    .filter(delta -> delta.i != 0 || delta.j != 0)
                    .map(delta -> new Coord(c.i + delta.i, c.j + delta.j))
                    .filter(it -> it.i >= 0 && it.i < n && it.j >= 0 && it.j < m);
        }

        void reset(Coord c) {
            set(c.i, c.j, 0);
        }
    }

    static class Coord {
        final int i, j;

        Coord(int i, int j) {
            this.i = i;
            this.j = j;
        }

        @Override
        public String toString() {
            return String.format("(%d, %d)", i, j);
        }

        @Override
        public int hashCode() {
            return Objects.hash(i, j);
        }

        @Override
        public boolean equals(Object o) {
            if (o.getClass() != Coord.class)
                return false;
            var oc = (Coord) o;
            return Objects.equals(i, oc.i) && Objects.equals(j, oc.j);
        }
    }
}
