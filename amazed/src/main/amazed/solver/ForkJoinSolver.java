package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Stack;
import java.util.HashMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveAction;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;
import java.util.concurrent.atomic.AtomicBoolean;
import java.util.concurrent.atomic.AtomicReference;



/**
 * <code>ForkJoinSolver</code> implements a solver for
 * <code>Maze</code> objects using a fork/join multi-thread
 * depth-first search.
 * <p>
 * Instances of <code>ForkJoinSolver</code> should be run by a
 * <code>ForkJoinPool</code> object.
 */

public class ForkJoinSolver
        extends SequentialSolver {
    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal.
     *
     * @param maze the maze to be searched
     */
    // boolean[] die;
    private List<ForkJoinTask<List<Integer>>> childThreads;
    private List<ForkJoinSolver> children = new ArrayList<ForkJoinSolver>();
    private ForkJoinPool pool;
    int ogStart;
    private boolean timeToDie;
    private static AtomicBoolean goalisFound = new AtomicBoolean();
    protected ConcurrentSkipListSet<Integer> visited;
    protected AtomicReference returnPath = new AtomicReference<>();


    public ForkJoinSolver(Maze maze) {
        super(maze);
        timeToDie=false;
    }


    /**
     * Creates a solver that searches in <code>maze</code> from the
     * start node to a goal, forking after a given number of visited
     * nodes.
     *
     * @param maze      the maze to be searched
     * @param forkAfter the number of steps (visited nodes) after
     *                  which a parallel task is forked; if
     *                  <code>forkAfter &lt;= 0</code> the solver never
     *                  forks new tasks
     */
    public ForkJoinSolver(Maze maze, int forkAfter) {
        this(maze);
        this.forkAfter = forkAfter;
        ogStart = start;
        goalisFound = new AtomicBoolean(false);
    }

    /*
     * public ForkJoinSolver(Maze maze, int forkAfter, int start,
     * ConcurrentSkipListSet visited, HashMap predecessor,
     * boolean[] die, int startStart) {
     * this(maze);
     * this.start = start;
     * this.visited = visited;
     * this.predecessor = predecessor;
     * this.die = die;
     * this.ogStart = startStart;
     * }
     */

    public ForkJoinSolver(Maze maze, int forkAfter, int start, ConcurrentSkipListSet visited, AtomicReference returnPath, AtomicBoolean goalisFound) {
        this(maze);
        this.start = start;
        this.forkAfter = forkAfter;
        this.visited = visited;
        this.returnPath = returnPath;
        this.goalisFound = goalisFound;
    }

    @Override
    protected void initStructures(){
        visited = new ConcurrentSkipListSet<>();
        predecessor = new HashMap<>();
        frontier = new Stack<>();
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreacheable), the method returns
     * <code>null</code>.
     *
     * @return the list of node identifiers from the start node to a
     *         goal node in the maze; <code>null</code> if such a path cannot
     *         be found.
     */
    public void canKillChildren(){
        timeToDie=true;
        System.out.println("TIME TO DIE!");
    }

    @Override
    public List<Integer> compute() {
        return parallelSearch();
        /*
         * List<Integer> returnValue = parallelSearch();
         * System.out.println(returnValue != null ? returnValue.toString() :
         * "NULL?!?!?");
         * if(start != ogStart) while(!timeToDie) {}
         * System.out.println("death solver = " + childThreads.size());
         * List<Integer> childValues = killChildThread(childThreads);
         * returnValue = (childValues != null) ? childValues : returnValue;
         * return returnValue;
         */
    }

    private List<Integer> parallelSearch() {
        //pool = new ForkJoinPool();

        // childThreads = new ArrayList<>();
        frontier.push(start);
        int player = maze.newPlayer(start);
        int numberOfSteps = 0;

        while (!frontier.empty() && !goalisFound.get()) {
            numberOfSteps++;

            int current = frontier.pop();

            if (visited.add(current)) {
                maze.move(player, current);

            if (maze.hasGoal(current)) {
                goalisFound.set(true);
                System.out.println("Found the goal!");
                // killChildThread(children);
                // Thread.sleep(1000); // plz no =(
                //returnPath.set(pathFromTo(maze.start(), current));
                //return pathFromTo(maze.start(), current);
                return pathFromTo(maze.start(), current);
            }
            // visited.add(current);
            int i = 0;
                for (int nb : maze.neighbors(current)) {
                    if (!visited.contains(nb)) {
                        predecessor.put(nb, current);

                        if (i == 0 /* || numberOfSteps < forkAfter */) {
                            frontier.push(nb);
                        }

                        else {
                            ForkJoinSolver child = new ForkJoinSolver(maze, forkAfter, nb, visited, returnPath, goalisFound);
                            children.add(child);
                            child.fork();
                        }
                        i++;
                    }
                }

            }

            // Issue #1: Det kan vara sa att vi maste anvanda forkpool
            // Issue #2: Det kan vara sa att den sitter och vantar pa ett returvarde nar man
            // kor beginCompute nedan. och darmed blir det ej parralell

            // jag hittad ett problem. nej det var att det fanns en kallelse paa die[0] kvar
            // i koden
            // Nu skickas inte resultatet tillbaks, men det krashar inte
            // testa kor igen
            // No worky =(
            // GO AGAIN
            // Vad är det du ändrar på? Jag la till ogStart igen, den hade vi slarvat bort.
            // sedan renskrev jag killchildren pga tänkte att den kanske var clutterd och
            // något var knasigt
            // Ja, problemet verkar ju vara att vi inte tar emot resultatet rätt
            /*
             * for (int i = 1; i < frontier.size(); i++) {
             *
             * //System.out.println("created a thread! at: " + (numberOfSteps));
             * ForkJoinTask task = new ForkJoinSolver(maze, forkAfter, frontier.pop(),
             * visited, predecessor, die, ogStart).fork();
             * childThreads.add(task);
             * //childThreads.add(new ForkJoinSolver(maze, forkAfter, frontier.pop(),
             * visited, predecessor, die, ogStart));
             * //childThreads.get(childThreads.size() - 1).fork();
             * System.out.println(childThreads.size());
             * }
             */

        }
        maze.move(player, ogStart);
        return killChildThread(); // Men liskom detta borde ju lösa det och fånga upp allt Det är inte detdär att
                                  // vi inte hinner fånga? eller var det en myt? ugh, right
                                  // Jag tänkte ta lunch nu, men ska vi sitta under mötet ikväll?
    }

    private List<Integer> killChildThread() {

        for (ForkJoinSolver child : children) {
            List<Integer> result = child.join();
            if (result != null) {
                break;
            }
        }
        return null;
        /*
         * List<Integer> toReturn = null;
         * System.out.println("killing smthn");
         * for (ForkJoinSolver fs : threads) {
         * //fs.canKillChildren();
         * //fs.join();
         * List<Integer> result = fs.join();
         * System.out.println(result == null ? "NULL" : "NOT NULL");
         * if (result != null) {
         * toReturn = result;
         * System.out.println("found a goal");
         * }
         * }
         * return toReturn;
         */
    }


}
