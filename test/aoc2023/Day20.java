package aoc2023;


import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

class Pulse {
    public static final boolean LOW = false;
    public static final boolean HIGH = true;
}

record SentPulse(String from, String to, boolean state) {
    @Override
    public String toString() {
        String p = state == Pulse.HIGH ? "high" : "low";
        return from + " -" + p + "-> " + to;
    }
}

interface Module {
    Queue<SentPulse> handlePulse(SentPulse pulse);

    List<String> getOutputs();

    String getId();

    Module copy();
}

class FlipFlop implements Module {
    private boolean state = Pulse.LOW;
    private final String id;
    private List<String> outputs;

    public FlipFlop(String id, List<String> outputs) {
        this.id = id;
        this.outputs = outputs;
    }

    @Override
    public Queue<SentPulse> handlePulse(SentPulse pulse) {
        LinkedList<SentPulse> sentPulses = new LinkedList<>();
        if (pulse.state()) {
            return sentPulses;
        }
        for (String m : outputs) {
            sentPulses.add(new SentPulse(this.id, m, !state));
        }
        this.state = !state;
        return sentPulses;
    }

    @Override
    public List<String> getOutputs() {
        return outputs;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public Module copy() {
        return new FlipFlop(id, new ArrayList<>(outputs));
    }

    @Override
    public String toString() {
        return "%" + id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        FlipFlop flipFlop = (FlipFlop) o;
        return state == flipFlop.state && Objects.equals(id, flipFlop.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(state, id);
    }
}

class Conjunction implements Module {

    private final String id;
    private List<String> output;
    private Map<String, Boolean> state;

    public Conjunction(String id, List<String> outputs) {
        this.id = id;
        this.output = outputs;
        this.state = new HashMap<>();
    }

    public void addInput(String input) {
        this.state.put(input, false);
    }

    @Override
    public Queue<SentPulse> handlePulse(SentPulse pulse) {
        if (state.containsKey(pulse.from())) {
            state.put(pulse.from(), pulse.state());
        }
        Set<Boolean> values = new HashSet<>(state.values());
        LinkedList<SentPulse> sentPulses = new LinkedList<>();
        boolean allConnectedAreHigh = values.size() == 1 && values.iterator().next();
        if (allConnectedAreHigh) {
            for (String m : output) {
                sentPulses.add(new SentPulse(this.id, m, Pulse.LOW));
            }
        } else {
            for (String m : output) {
                sentPulses.add(new SentPulse(this.id, m, Pulse.HIGH));
            }
        }
        return sentPulses;
    }

    @Override
    public List<String> getOutputs() {
        return output;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public Module copy() {
        Conjunction conjunction = new Conjunction(id, new ArrayList<>(output));
        conjunction.state = new HashMap<>(state);
        return conjunction;
    }

    @Override
    public String toString() {
        return "&" + id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Conjunction that = (Conjunction) o;
        return Objects.equals(id, that.id) && Objects.equals(state, that.state);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id, state);
    }
}

class Broadcast implements Module {

    public static final String ID = "broadcaster";
    private final String id;
    private List<String> outputs;

    public Broadcast(String id, List<String> outputs) {
        this.id = id;
        this.outputs = outputs;
    }

    @Override
    public Queue<SentPulse> handlePulse(SentPulse pulse) {
        LinkedList<SentPulse> sentPulses = new LinkedList<>();
        for (String m : outputs) {
            sentPulses.add(new SentPulse(id, m, pulse.state()));
        }
        return sentPulses;
    }

    @Override
    public List<String> getOutputs() {
        return outputs;
    }

    @Override
    public String getId() {
        return id;
    }

    @Override
    public Module copy() {
        return new Broadcast(id, new ArrayList<>(outputs));
    }

    @Override
    public String toString() {
        return id;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        return true;
    }

    @Override
    public int hashCode() {
        return 1;
    }
}

class Button implements Module {
    private String broadcast;

    public Button(String broadcast) {
        this.broadcast = broadcast;
    }

    @Override
    public Queue<SentPulse> handlePulse(SentPulse pulse) {
        LinkedList<SentPulse> sentPulses = new LinkedList<>();
        sentPulses.add(new SentPulse(toString(), broadcast, Pulse.LOW));
        return sentPulses;
    }

    @Override
    public List<String> getOutputs() {
        return List.of(broadcast);
    }

    @Override
    public String getId() {
        return toString();
    }

    @Override
    public Module copy() {
        return new Button(broadcast);
    }

    @Override
    public String toString() {
        return "button";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Button button = (Button) o;
        return Objects.equals(broadcast, button.broadcast);
    }

    @Override
    public int hashCode() {
        return Objects.hash(broadcast);
    }
}

class Output implements Module {

    private final String id;

    public Output(String id) {
        this.id = id;
    }

    @Override
    public Queue<SentPulse> handlePulse(SentPulse pulse) {
//        System.err.println(pulse);
        return new LinkedList<>();
    }

    @Override
    public List<String> getOutputs() {
        return Collections.emptyList();
    }

    @Override
    public String toString() {
        return "output";
    }

    @Override
    public String getId() {
        return toString();
    }

    @Override
    public Module copy() {
        return new Output(id);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Output output = (Output) o;
        return Objects.equals(id, output.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}

public class Day20 {

    private static final Map<Map<String, Module>, Map<String, Module>> cache = new HashMap<>();
    private static long pushNr = 0;
    private static Map<String, Module> currentModules = new HashMap<>();

    public static void main(String[] args) throws Exception {
//        String input = example2;
        String input = Files.readString(Path.of("resources/day20.txt"));
        Map<String, Module> modules = parse(input);
        Module button = modules.get("button");
//        int part1 = pushTimes(1000, button, modules);
//        System.err.println("part1 = " + part1);//730797576

        modules = parse(input);
        currentModules = modules;
        pushNr = 0;
        for (int i = 0; i < 1000000; i++) {
            pushTimes2(1000000, button, currentModules);
        }
    }

    private static Map<String, Module> copy(Map<String, Module> modules) {
        Map<String, Module> copy = new HashMap<>();
        for (Module m : modules.values()) {
            copy.put(m.getId(), m.copy());
        }
        return copy;
    }

    private static int pushTimes(int n, Module button, Map<String, Module> modules) {
        int low = 0;
        int high = 0;
        for (int i = 0; i < n; i++) {
//            System.err.println(" PUSH " + (i + 1));
            Pair<Integer> result = push(button, modules);
            low += result.first;
            high += result.second;
//            System.err.println("\n\n");
        }
        return low * high;
    }

    private static Pair<Integer> push(Module button, Map<String, Module> modules) {
        Queue<SentPulse> q = new LinkedList<>(button.handlePulse(null));
        int lowCount = 0;
        int highCount = 0;
        while (!q.isEmpty()) {
            SentPulse pulse = q.poll();
//            System.err.println(pulse);
            if (pulse.state()) {
                highCount++;
            } else {
                lowCount++;
            }
            Module target = modules.getOrDefault(pulse.to(), new Output(pulse.to()));
            q.addAll(target.handlePulse(pulse));
        }


        return new Pair<>(lowCount, highCount);
    }

    private static void pushTimes2(int n, Module button, Map<String, Module> modules) {
        for (int i = 0; i < n; i++) {
            push2(button, modules);
        }
    }

    private static void push2(Module button, Map<String, Module> modules) {
        if (pushNr % 10000000 == 0) {
            System.err.println("PUSH " + pushNr);
        }
        Map<String, Module> cacheEntry = cache.get(modules);
        if (cacheEntry != null) {
            currentModules = copy(cacheEntry);
            pushNr++;
            System.err.println(pushNr + " CACHE HIT !!!!");
            return;
        }

//        Map<String, Module> input = copy(modules);

        Queue<SentPulse> q = new LinkedList<>(button.handlePulse(null));
        while (!q.isEmpty()) {
            SentPulse pulse = q.poll();
//            System.err.println(pulse);
            Module target = modules.getOrDefault(pulse.to(), new Output(pulse.to()));
            if (pulse.to().equals("rx") && !pulse.state()) {
                System.err.println("**************************************** RX pushes = " + pushNr + " ****************************************");
                throw new IllegalArgumentException(" FOUND part 2 = " + pushNr);
            }
            q.addAll(target.handlePulse(pulse));

//            cacheEntry = cache.get(modules);
//            if (cacheEntry != null) {
////                System.err.println(pushNr + " CACHE HIT !!!!");
//                currentModules = copy(cacheEntry);
//                pushNr++;
//                return;
//            }
        }
        pushNr++;
//        cache.put(input, copy(modules));
//        System.err.println(cache.size());
    }

    private static Map<String, Module> parse(String input) {
        String[] lines = input.split("\n");
        Map<String, Module> modules = new HashMap<>();
        modules.put("button", new Button(Broadcast.ID));
        for (String line : lines) {
            String[] parts = line.split(" -> ");
            String name = parts[0];
            List<String> connected = Arrays.stream(parts[1].split(",")).map(String::trim).toList();
            if (name.equals(Broadcast.ID)) {
                modules.put(Broadcast.ID, new Broadcast(Broadcast.ID, connected));
            } else {
                String type = name.substring(0, 1);
                String id = name.substring(1);
                if (type.equals("%")) {
                    modules.put(id, new FlipFlop(id, connected));
                }
                if (type.equals("&")) {
                    modules.put(id, new Conjunction(id, connected));
                }
            }
        }

        Set<Conjunction> conjunctions = modules.values().stream().filter(m -> m instanceof Conjunction).map(m -> (Conjunction) m).collect(Collectors.toSet());
        for (Module module : modules.values()) {
            for (Conjunction conjunction : conjunctions) {
                if (module.getOutputs().contains(conjunction.getId())) {
                    conjunction.addInput(module.getId());
                }
            }
        }
        return modules;
    }


    static String example1 = "broadcaster -> a, b, c\n" +
            "%a -> b\n" +
            "%b -> c\n" +
            "%c -> inv\n" +
            "&inv -> a";

    static String example2 = "broadcaster -> a\n" +
            "%a -> inv, con\n" +
            "&inv -> b\n" +
            "%b -> con\n" +
            "&con -> output";

    record Pair<T>(T first, T second) {
    }
}
