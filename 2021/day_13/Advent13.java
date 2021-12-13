import java.io.*;
import java.util.stream.*;
import java.util.*;

public class Advent13 {
    public static void main(String[] args) throws IOException {
        var lines = new BufferedReader(new InputStreamReader(System.in))
                .lines()
                .filter(l -> !l.isEmpty())
                .collect(Collectors.toList());
        Set<List<Integer>> dots = lines.stream()
                .filter(line -> line.contains(","))
                .map(line -> line.split(","))
                .map(it -> Arrays.asList(Integer.parseInt(it[0]), Integer.parseInt(it[1])))
                .collect(Collectors.toSet());
        List<List<Integer>> folds = lines.stream()
                .filter(line -> line.contains("fold"))
                .map(line -> line.split("\\s")[2].split("="))
                .map(it -> it[0].equals("x")
                                ? Arrays.asList(Integer.parseInt(it[1]), 0)
                                : Arrays.asList(0, Integer.parseInt(it[1])))
                .collect(Collectors.toList());

        Set<List<Integer>> folded = new HashSet();
        for (List<Integer> fold : folds) {
            for (List<Integer> dot : dots) {
                List<Integer> folDot = fold(dot, fold);
                folded.remove(dot);
                folded.add(folDot);
            }
            dots = folded;
            folded = new HashSet<>();
        }
        print(dots);
    }

    private static void print(Set<List<Integer>> dots) {
        Integer maxX = dots.stream().map(it -> it.get(0)).max(Comparator.naturalOrder()).orElse(0);
        Integer maxY = dots.stream().map(it -> it.get(1)).max(Comparator.naturalOrder()).orElse(0);

        StringBuilder sb = new StringBuilder();
        sb.append("\n");
        for (int y = 0; y <= maxY; y++) {
            for (int x = 0; x <= maxX; x++) {
                sb.append(dots.contains(Arrays.asList(x, y)) ? "#" : ".");
            }
            sb.append("\n");
        }
        System.out.println(sb.toString());
    }

    private static List<Integer> fold(List<Integer> dot, List<Integer> axis) {
        return Arrays.asList(fold(dot.get(0), axis.get(0)), fold(dot.get(1), axis.get(1)));
    }

    private static Integer fold(Integer coord, Integer pivot) {
        return pivot == 0 ? coord : pivot - Math.abs(coord - pivot);
    }
}

