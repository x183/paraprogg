package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.concurrent.ConcurrentSkipListSet;

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
    boolean[] die;
    int ogStart;

    public ForkJoinSolver(Maze maze) {
        super(maze);
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
        die = new boolean[] { false };
        ogStart = start;
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
    public List<Integer> beginCompute(int start, Set visited, Map predecessor, boolean[] die, int startStart) {
        this.start = start;
        this.visited = visited;
        this.predecessor = predecessor;
        this.die = die;
        this.ogStart = startStart;
        return compute();
    }

    @Override
    public List<Integer> compute() {
        return parallelSearch();
    }

    private List<Integer> parallelSearch() {
        List<ForkJoinSolver> solver = new ArrayList<>();
        frontier.push(start);
        int player = maze.newPlayer(start);
        int numberOfSteps = 0;
        while (!frontier.empty()) {
            if (die[0]) {
                List<Integer> toReturn = null;
                for (ForkJoinSolver fs : solver) {
                    List<Integer> result = fs.join();
                    if (result != null) {
                        toReturn = result;
                    }
                }
                System.out.println("killed some threads");
                maze.move(player, ogStart);

                return toReturn;
            }
            int current = frontier.pop();

            if (maze.hasGoal(current)) {
                die[0] = true;
                for (ForkJoinSolver fs : solver) {
                    fs.join();
                }
                maze.move(player, current);
                return pathFromTo(ogStart, current);
            }

            if (!visited.contains(current)) {
                maze.move(player, current);
                visited.add(current);
                for (int nb : maze.neighbors(current)) {
                    if (!visited.contains(nb)) {
                        frontier.push(nb);
                        predecessor.put(nb, current);
                    }
                }
            }

            if (forkAfter != 0 && ++numberOfSteps % forkAfter == 0) {
                solver.add(new ForkJoinSolver(maze, forkAfter));
                solver.get(solver.size() - 1).fork();
                solver.get(solver.size() - 1).beginCompute(frontier.pop(), visited, predecessor, die, ogStart);
            }

        }
        System.out.println("reached end");
        maze.move(player, ogStart);
        return null;
    }
}
