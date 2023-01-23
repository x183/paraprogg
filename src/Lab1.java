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
    Point switch1 = new Point(17, 7);
    Point switch2 = new Point(15, 9);
    Point switch3 = new Point(4, 9);
    Point switch4 = new Point(3, 11);
    pathList.add(new Path(15, 3, 16, 7, 1, new Point[] { switch1, null }));
    pathList.add(new Path(15, 5, 16, 8, 2, new Point[] { switch1, null }));
    pathList.add(new Path(18, 7, 16, 9, 3, new Point[] { switch2, switch1 }));
    pathList.add(new Path(14, 9, 5, 9, 4, new Point[] { switch3, switch2 }));
    pathList.add(new Path(14, 10, 5, 10, 5, new Point[] { switch3, switch2 }));
    pathList.add(new Path(3, 9, 2, 11, 6, new Point[] { switch4, switch3 }));
    pathList.add(new Path(4, 11, 15, 11, 7, new Point[] { null, switch4 }));
    pathList.add(new Path(4, 13, 15, 13, 8, new Point[] { null, switch4 }));

    Train train1 = new Train(1, speed1, tsi, pathList);
    Train train2 = new Train(2, speed2, tsi, pathList);

    Thread thread1 = new Thread(train1);
    Thread thread2 = new Thread(train2);

    thread1.start();
    thread2.start();

  }
}

// Critical: 3,6
// Parallel: 1,2,4,5,7,8

class Train implements Runnable {
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
        newSensorEventReceived(sensorE);
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

  private void newSensorEventReceived(TSim.SensorEvent sensorE) throws InterruptedException, CommandException {
    for (Path p : pathList) {
      for (Point point : p.pos) {

        if (!((point.getX() == sensorE.getXpos()) && (point.getY() == sensorE.getYpos()))) {
          continue;
        }

        Semaphore busy = p.busy;

        if (p.index == currentPath) {
          busy.release();
          int nexPath = getNextDesiredPath(currentPath);
          if (nexPath == 3 || nexPath == 6) { // Critical roads
            // stanna och vänta eventuellt
          } else if (nexPath == 1 || nexPath == 2 || nexPath == 7 || nexPath == 8) { // Train station
            sleepAtStation();
            // Turn the train osv
          } else { // Paralell roads
            setSwitch(nexPath);
          }
          break;
        }
        if (busy.tryAcquire(1, TimeUnit.MILLISECONDS)) {
          busy.acquire(); // should be something else
          currentPath = p.index;
          break;
        }
        tsi.setSpeed(trainId, 0);
        busy.acquire();
        tsi.setSpeed(trainId, trainSpeed);
        // wait
      }
    }
  }

  private void setSwitch(int nexPath) throws CommandException {
    Path path = (Path) (pathList.stream().filter(c -> (c.index == currentPath))).toArray()[0];
    Point sw = path.attachedSwitches[TowardsStation2 ? 0 : 1];
    if (currentPath == 1 && nexPath == 3 ||
        currentPath == 3 && nexPath == 1 ||
        currentPath == 3 && nexPath == 4 ||
        currentPath == 4 && nexPath == 3 ||
        currentPath == 5 && nexPath == 6 ||
        currentPath == 6 && nexPath == 5 ||
        currentPath == 6 && nexPath == 8 ||
        currentPath == 8 && nexPath == 6) {
      tsi.setSwitch(sw.x, sw.y, tsi.SWITCH_RIGHT);

    }
    else {
      tsi.setSwitch(sw.x, sw.y, tsi.SWITCH_LEFT);
    }

  }

  private int getNextDesiredPath(int currentPath) {
    return ((currentPath + (TowardsStation2 ? 0 : -2)) % 8) + 1;
  }

  private void sleepAtStation() throws CommandException, InterruptedException {
    tsi.setSpeed(trainId, 0);
    trainSpeed *= -1;
    Thread.sleep(1500);
    tsi.setSpeed(trainId, trainSpeed);
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
  public Point[] attachedSwitches;

  public Path(int x1, int y1, int x2, int y2, int index, Point[] attachedSwitches) {
    pos = new Point[] { new Point(x1, y1), new Point(x2, y2) };
    busy = new Semaphore(1);
    this.index = index;
    this.attachedSwitches = attachedSwitches;
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