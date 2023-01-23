import TSim.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

public class Lab1 {
  List<Path> pathList;

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    // Adds all separate paths to the lists
    pathList = new ArrayList<>();
    pathList.add(new Path(15, 3, 16, 7, 1));
    pathList.add(new Path(15, 5, 16, 8, 2));
    pathList.add(new Path(18, 7, 16, 9, 3));
    pathList.add(new Path(14, 9, 5, 9, 4));
    pathList.add(new Path(14, 10, 5, 10, 5));
    pathList.add(new Path(3, 9, 2, 11, 6));
    pathList.add(new Path(4, 11, 15, 11, 7));
    pathList.add(new Path(4, 13, 15, 13, 8));
    Train train1 = new Train(1, speed1, tsi, pathList);
    Train train2 = new Train(2, speed2, tsi, pathList);
    Thread thread1 = new Thread(train1);
    Thread thread2 = new Thread(train2);
    thread1.start();
    thread2.start();

  }
}

class Train implements Runnable {
  public static final int NONE = 0;
  int trainSpeed;
  int trainId;
  int currentPath = 0; // 0 = null
  boolean TowardsStation2;
  List<Path> pathList;
  TSimInterface tsi;
  // Semaphore paths = new Semaphore(1);

  @Override
  public void run() {
    try {
      tsi.setSpeed(trainId, trainSpeed);
      while (true) {
        TSim.SensorEvent sensorE = tsi.getSensor(trainId);
        // If not have semaphore and is free, take semaphore, else take detour
        // If have semaphore during event, return semaphore
        for (Path p : pathList) {
          for (Point point : p.pos) {
            if (!((point.getX() == sensorE.getXpos()) && (point.getY() == sensorE.getYpos()))) {
              continue;
            }
            Semaphore busy = p.busy;
            if (p.index == currentPath) {
              busy.release();
              currentPath = NONE;
              break;
            }
            if (busy.tryAcquire(1, TimeUnit.MILLISECONDS)) {
              busy.acquire(); // should be something else
              currentPath = p.index;
              break;
            }
            // Take detour if possible, else wait

          }
        }
      }
    } catch (

    CommandException e) {
      e.printStackTrace(); // or only e.getMessage() for the error
      System.exit(1);
    } catch (InterruptedException e) {
      e.printStackTrace();
      System.out.println("The program has been interrupted");
      System.exit(1);
    }

  }

  public Train(int trainId, int trainSpeed, TSimInterface tsi, List<Path> pathList) {
    this.trainId = trainId;
    this.trainSpeed = trainSpeed;
    this.tsi = tsi;
    this.pathList = pathList;
  }

}

class Path {
  public int index;
  public Point[] pos;
  public Semaphore busy;

  public Path(int x1, int y1, int x2, int y2, int index) {
    pos = new Point[] { new Point(x1, y1), new Point(x2, y2) };
    busy = new Semaphore(1);
    this.index = index;
  }
}

class Point {
  int x;
  int y;

  public Point(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public int getX() {
    return this.x;
  }

  public int getY() {
    return this.y;
  }
}