package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Stack;
import java.util.HashMap;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
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
    protected AtomicReference<List<Integer>> returnPath = new AtomicReference<>();

    private List<Integer> path = new ArrayList<>();


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


    public ForkJoinSolver(Maze maze, int forkAfter, int start, ConcurrentSkipListSet visited, AtomicBoolean goalisFound, List<Integer> path) {
        this(maze);
        this.start = start;
        this.forkAfter = forkAfter;
        this.visited = visited;
        this.goalisFound = goalisFound;
        this.path = path;
    }

    @Override
    protected void initStructures(){
        visited = visited != null ? visited : new ConcurrentSkipListSet<>();
        predecessor = new HashMap<>();
        frontier = new Stack<>();
    }

    /**
     * Searches for and returns the path, as a list of node
     * identifiers, that goes from the start node to a goal node in
     * the maze. If such a path cannot be found (because there are no
     * goals, or all goals are unreachable), the method returns
     * <code>null</code>.
     *
     * @return the list of node identifiers from the start node to a
     *         goal node in the maze; <code>null</code> if such a path cannot
     *         be found.
     */

    @Override
    public List<Integer> compute() {
        return parallelSearch();

    }

    private List<Integer> parallelSearch() {
        //pool = new ForkJoinPool();

        // childThreads = new ArrayList<>();
        frontier.push(start);
        int player = maze.newPlayer(start);
        int numberOfSteps = 0;

        int last = 0;
        List<Integer> tpath = new ArrayList<>();
        while (!frontier.empty() && !goalisFound.get()) {
            int current = frontier.pop();

            path.add(current);
            if (visited.add(current) || start == current) {
                maze.move(player, current);

            if (maze.hasGoal(current)) {
                goalisFound.set(true);
                path.addAll(tpath);
                System.out.println("Found the goal!");
                return path;
            }
            int i = 0;
                for (int nb : maze.neighbors(current)) {
                    if (!visited.contains(nb)) {
                        if (i == 0) {
                            frontier.push(nb);
                        }

                        else {
                            numberOfSteps = 0;

                            if (visited.add(nb)) {
                                ForkJoinSolver child = new ForkJoinSolver(maze, forkAfter, nb, visited, goalisFound,
                                        new ArrayList<Integer>(path));
                            children.add(child);
                            child.fork();
                        }
                    }
                        i++;
                    }
                }
                numberOfSteps++;
            }



        }
        return killChildren();
    }

    private List<Integer> killChildren() {

        for (ForkJoinSolver child : children) {
            List<Integer> result = child.join();
            if (result != null) {
                return result;
            }
        }
        return null;

    }


}
