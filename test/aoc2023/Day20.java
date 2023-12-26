package aoc2023;


import java.math.BigDecimal;
import java.math.BigInteger;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.stream.Collectors;

class Pulse {
    public static final boolean LOW = false;
    public static final boolean HIGH = true;
}

record PulseTick(String from, String to, boolean state) {
    @Override
    public String toString() {
        String p = state == Pulse.HIGH ? "high" : "low";
        return from + " -" + p + "-> " + to;
    }
}

interface Module {
    Queue<PulseTick> handlePulse(PulseTick pulse);

    List<String> getOutputs();

    String getId();

    Module copy();

    String getState();

    List<String> getInputs();

    void addInput(String input);
}

class FlipFlop implements Module {
    private boolean state = Pulse.LOW;
    private final String id;
    private List<String> outputs;

    private List<String> inputs = new ArrayList<>();

    public FlipFlop(String id, List<String> outputs) {
        this.id = id;
        this.outputs = outputs;
    }

    public void addInput(String input) {
        this.inputs.add(input);
    }

    @Override
    public Queue<PulseTick> handlePulse(PulseTick pulse) {
        LinkedList<PulseTick> pulsTicks = new LinkedList<>();
        if (pulse.state()) {
            return pulsTicks;
        }
        this.state = !state;
        for (String m : outputs) {
            pulsTicks.add(new PulseTick(this.id, m, state));
        }
        return pulsTicks;
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
    public String getState() {
        return state ? "-H-" : "-L-";
    }

    @Override
    public List<String> getInputs() {
        return inputs;
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
        return Objects.equals(id, flipFlop.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}

class Conjunction implements Module {

    private final String id;
    private List<String> outputs;
    private Map<String, Boolean> state;
    private List<String> inputs = new ArrayList<>();


    public Conjunction(String id, List<String> outputs) {
        this.id = id;
        this.outputs = outputs;
        this.state = new HashMap<>();
    }

    public void addInput(String input) {
        this.state.put(input, false);
        this.inputs.add(input);
    }

    @Override
    public Queue<PulseTick> handlePulse(PulseTick pulse) {
        state.put(pulse.from(), pulse.state());
        LinkedList<PulseTick> pulsTicks = new LinkedList<>();
        boolean inputsHigh = areAllInputsHigh();
        if (inputsHigh) {
            for (String m : outputs) {
                pulsTicks.add(new PulseTick(this.id, m, Pulse.LOW));
            }
        } else {
            for (String m : outputs) {
                pulsTicks.add(new PulseTick(this.id, m, Pulse.HIGH));
            }
        }
        return pulsTicks;
    }

    private boolean areAllInputsHigh() {
        Set<Boolean> values = new HashSet<>(state.values());
        return values.size() == 1 && values.iterator().next();
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
        Conjunction conjunction = new Conjunction(id, new ArrayList<>(outputs));
        conjunction.state = new HashMap<>(state);
        return conjunction;
    }

    @Override
    public String getState() {
        return areAllInputsHigh() ? "-H-" : "-L-";
    }

    @Override
    public List<String> getInputs() {
        return inputs;
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
        return Objects.equals(id, that.id);
    }

    @Override
    public int hashCode() {
        return Objects.hash(id);
    }
}

class Broadcast implements Module {

    public static final String ID = "broadcaster";
    private final String id;
    private List<String> outputs;
    private List<String> inputs = new ArrayList<>();

    public Broadcast(String id, List<String> outputs) {
        this.id = id;
        this.outputs = outputs;
    }

    public void addInput(String input) {
        this.inputs.add(input);
    }

    @Override
    public Queue<PulseTick> handlePulse(PulseTick pulse) {
        LinkedList<PulseTick> pulsTicks = new LinkedList<>();
        for (String m : outputs) {
            pulsTicks.add(new PulseTick(id, m, pulse.state()));
        }
        return pulsTicks;
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
    public String getState() {
        return "---";
    }

    @Override
    public List<String> getInputs() {
        return inputs;
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
    private List<String> inputs = new ArrayList<>();

    public Button(String broadcast) {
        this.broadcast = broadcast;
    }

    public void addInput(String input) {
        this.inputs.add(input);
    }

    @Override
    public Queue<PulseTick> handlePulse(PulseTick pulse) {
        LinkedList<PulseTick> pulsTicks = new LinkedList<>();
        pulsTicks.add(new PulseTick(toString(), broadcast, Pulse.LOW));
        return pulsTicks;
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
    public String getState() {
        return "---";
    }

    @Override
    public List<String> getInputs() {
        return inputs;
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
    private List<String> inputs = new ArrayList<>();

    public Output(String id) {
        this.id = id;
    }

    public void addInput(String input) {
        this.inputs.add(input);
    }

    @Override
    public Queue<PulseTick> handlePulse(PulseTick pulse) {
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
        return id;
    }

    @Override
    public Module copy() {
        return new Output(id);
    }

    @Override
    public String getState() {
        return "---";
    }

    @Override
    public List<String> getInputs() {
        return inputs;
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

    private static long pushNr = 0;

    static double lcm(long a, long b) {
        return Math.abs(a * b) / BigInteger.valueOf(a).gcd(BigInteger.valueOf(b)).doubleValue();
    }

    static double lcm(List<Long> numbers) {
        return numbers.stream().reduce((a, b) -> (long) lcm(a, b)).get();
    }


    public static void main(String[] args) throws Exception {
//        String input = example2;
        String input = Files.readString(Path.of("resources/day20.txt"));
        Map<String, Module> modules = parse(input);
        Module button = modules.get("button");
        int part1 = pushTimes(1000, button, modules);
        System.err.println("part1 = " + part1);//730797576

        Set<String> moduleIds = ancestors(2, Set.of(modules.get("rx")), modules);
        List<Long> cycles = new ArrayList<>();
        for (String id : moduleIds) {
            modules = parse(input);
            Long push = pushTimes(button, modules, modules.get(id));
            cycles.add(push);
        }
        double part2 = lcm(cycles);
        System.err.println("part2 = " + BigDecimal.valueOf(part2).toPlainString());//226732077152351
    }

    private static int pushTimes(int n, Module button, Map<String, Module> modules) {
        int low = 0;
        int high = 0;
        for (int i = 0; i < n; i++) {
            Pair<Integer> result = push(button, modules);
            low += result.first();
            high += result.second();
        }
        return low * high;
    }

    private static Pair<Integer> push(Module button, Map<String, Module> modules) {
        Queue<PulseTick> q = new LinkedList<>(button.handlePulse(null));
        int lowCount = 0;
        int highCount = 0;
        while (!q.isEmpty()) {
            PulseTick pulse = q.poll();
            if (pulse.state()) {
                highCount++;
            } else {
                lowCount++;
            }
            Module target = modules.get(pulse.to());
            q.addAll(target.handlePulse(pulse));
        }
        return new Pair<>(lowCount, highCount);
    }

    private static Long pushTimes(Module button, Map<String, Module> modules, Module m) {
        pushNr = 0;
        for (int i = 0; i < 5000; i++) {
            Long nr = push(button, modules, m);
            if (nr != null) {
                return nr;
            }
        }
        return null;
    }

    private static Long push(Module button, Map<String, Module> modules, Module m) {
        pushNr++;
        Queue<PulseTick> q = new LinkedList<>(button.handlePulse(null));
        while (!q.isEmpty()) {
            PulseTick pulse = q.poll();
            Module target = modules.get(pulse.to());
            q.addAll(target.handlePulse(pulse));
            if (m != null) {
                if (target.equals(m) && !pulse.state()) {
                    return pushNr;
                }
            }
        }
        return null;
    }

    private static Set<String> ancestors(int level, Set<Module> m, Map<String, Module> modules) {
        if (level == 0) {
            return m.stream().map(Module::getId).collect(Collectors.toSet());
        }
        Set<String> levelModules = m.stream().flatMap(i -> i.getInputs().stream()).collect(Collectors.toSet());
        return ancestors(level - 1, levelModules.stream().map(modules::get).collect(Collectors.toSet()), modules);
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

        Set<String> allModuleIds = modules.values().stream().flatMap(m -> m.getOutputs().stream()).collect(Collectors.toSet());
        for (String id : allModuleIds) {
            if (!modules.containsKey(id)) {
                modules.put(id, new Output(id));
            }
        }

        for (Module module : modules.values()) {
            Set<Module> children = module.getOutputs().stream().map(modules::get).collect(Collectors.toSet());
            for (Module child : children) {
                child.addInput(module.getId());
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
}
