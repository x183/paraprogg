package amazed.solver;

import amazed.maze.Maze;

import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.Stack;
import java.util.concurrent.ConcurrentSkipListSet;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveAction;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentMap;

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
    List<ForkJoinTask<List<Integer>>> childThreads;
    ForkJoinPool pool;
    int ogStart;
    private boolean timeToDie;

    protected ConcurrentSkipListSet<Integer> visited;
    protected ConcurrentMap<Integer, Integer> predecessor;

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
        die = new boolean[] { false };
        ogStart = start;
        timeToDie=false;
    }

    public ForkJoinSolver(Maze maze, int forkAfter, int start, ConcurrentSkipListSet visited, ConcurrentMap predecessor, boolean[] die, int startStart){
        this(maze);
        this.start = start;
        this.visited = visited;
        this.predecessor = predecessor;
        this.die = die;
        this.ogStart = startStart;
}
    
    @Override
    protected void initStructures(){
        visited = new ConcurrentSkipListSet<>();
        predecessor = new ConcurrentHashMap<>();
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
        List<Integer> returnValue = parallelSearch();
        System.out.println(returnValue != null ? returnValue.toString() : "NULL?!?!?");
        if(start != ogStart) while(!timeToDie) {}
        System.out.println("death solver = " + childThreads.size());
        List<Integer> childValues = killChildThread(childThreads);
        returnValue = (childValues != null) ? childValues : returnValue;
        return returnValue;
    }

    private List<Integer> parallelSearch() {
        //pool = new ForkJoinPool();

        childThreads = new ArrayList<>();
        frontier.push(start);
        int player = maze.newPlayer(start);
        int numberOfSteps = 0;
        while (!frontier.empty()) {
            
            if (die[0]) {
                canKillChildren();
                return null;
            }

            int current = frontier.pop();
            //System.out.println(current);
            //System.out.println(maze.neighbors(current).toString());

            if (maze.hasGoal(current)) {
                die[0] = true;
                maze.move(player, current);
                System.out.println("Found the goal!");
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

            // Issue #1: Det kan vara så att vi måste använda forkpool
            // Issue #2: Det kan vara så att den sitter och väntar på ett returvärde när man kör beginCompute nedan. och därmed blir det ej parralellt
            numberOfSteps++;
            if (forkAfter != 0 && numberOfSteps % forkAfter == 0) {
                if(frontier.size() > 1){
                    //System.out.println("created a thread! at: " + (numberOfSteps));
                    ForkJoinTask task = new ForkJoinSolver(maze, forkAfter, frontier.pop(), visited, predecessor, die, ogStart).fork();
                    childThreads.add(task);
                    //childThreads.add(new ForkJoinSolver(maze, forkAfter, frontier.pop(), visited, predecessor, die, ogStart));
                    //childThreads.get(childThreads.size() - 1).fork();
                    System.out.println(childThreads.size());
                }
                else if (frontier.size() == 1) numberOfSteps--;
            }


        }
        maze.move(player, ogStart);
        return null;
    }

    private class PSolver extends RecursiveAction{
        private int start;
        private ConcurrentSkipListSet visited;
        private ConcurrentMap predecessor;
        private boolean[] die;
        private int startStart;

        @Override
        public void compute(){
        }
    }

    private List<Integer> killChildThread(List<ForkJoinTask<List<Integer>>> threads) {
        List<Integer> toReturn = null;
        System.out.println("killing smthn");
        for (ForkJoinTask<List<Integer>> fs : threads) {
            //fs.canKillChildren();
            //fs.join();
            List<Integer> result = fs.join();
            System.out.println(result == null ? "NULL" : "NOT NULL");
            if (result != null) {
                toReturn = result;
                System.out.println("found a goal");
            }
        }
        return toReturn;
    }   


}
