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
        System.out.println("TrainId: " + trainId + "\n\tsensorId: " + sensorE);
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
          Point toRelease = p.pos[(TowardsStation2 ? 1 : 0)];
          if (toRelease.getX() == sensorE.getXpos() && toRelease.getY() == sensorE.getYpos()) {
            busy.release();
          }
          int nextPath = getNextDesiredPath();
          final int lambdaNextPath = nextPath;
          Path path = (Path) (pathList.stream().filter(c -> (c.index == lambdaNextPath))).toArray()[0];
          Semaphore nextBusy = path.busy;
          if (nextPath == 3 || nextPath == 6) { // Critical roads
            if (!nextBusy.tryAcquire()) {
              // tsi.setSpeed(trainId, 0);
              nextBusy.acquire();
              // tsi.setSpeed(trainId, trainSpeed);
            }
            // stanna och vänta eventuellt
          } else { // Intersection
            if (!nextBusy.tryAcquire(5, TimeUnit.MILLISECONDS)) {
              nextPath++;
            } else {
            }

            if (nextPath == 1 || nextPath == 2 || nextPath == 7 || nextPath == 8) { // Station
            } else {

            }
            // sleepAtStation();
            // Turn the train osv
          }
          setSwitch(nextPath);
          break;
        }
        // currentPath = p.index;
        if (busy.tryAcquire(5, TimeUnit.MILLISECONDS)) {
          System.out.println("Reached not owned");
          currentPath = p.index;
          int next = getNextDesiredPath();
          setSwitch(next);

          break;
        }
        // wait
      }
    }
  }

  private void setSwitch(int nextPath) throws CommandException {
    System.out.println("Reached setSwitch\n\t NextPath: " + nextPath + "\n\tCurrent Path: " + currentPath);
    Path path = (Path) (pathList.stream().filter(c -> (c.index == currentPath))).toArray()[0];
    Point sw = path.attachedSwitches[TowardsStation2 ? 0 : 1];
    if (currentPath == 1 && nextPath == 3 ||
        currentPath == 3 && nextPath == 1 ||
        currentPath == 3 && nextPath == 4 ||
        currentPath == 4 && nextPath == 3 ||
        currentPath == 5 && nextPath == 6 ||
        currentPath == 6 && nextPath == 5 ||
        currentPath == 6 && nextPath == 8 ||
        currentPath == 8 && nextPath == 6) {
      tsi.setSwitch(sw.x, sw.y, tsi.SWITCH_RIGHT);
    }
    else {
      tsi.setSwitch(sw.x, sw.y, tsi.SWITCH_LEFT);
    }
  }

  private int getNextDesiredPath() {
    int nextPath = 0;
    if (TowardsStation2) {
      switch (currentPath) {
        case 1:
        case 2:
          nextPath = 3;
          break;
        case 3:
          nextPath = 4;
          break;
        case 4:
        case 5:
          nextPath = 6;
          break;
        case 6:
        case 7:
          nextPath = 7;
          break;
        case 8:
        default:
          nextPath = 8;
          break;
      }
    }

    else {
      switch (currentPath) {
        case 1:
        case 3:
          nextPath = 1;
          break;
        case 2:
          nextPath = 2;
          break;
        case 4:
        case 5:
          nextPath = 3;
          break;
        case 6:
          nextPath = 4;
          break;
        case 7:
        case 8:
        default:
          nextPath = 6;
          break;
      }
    }
    return nextPath;

  }

  private void sleepAtStation() throws CommandException, InterruptedException {
    System.out.println("Sleepy sleep");
    tsi.setSpeed(trainId, 0);
    trainSpeed *= -1;
    Thread.sleep(1500);
    TowardsStation2 = !TowardsStation2;
    tsi.setSpeed(trainId, trainSpeed);
  }


  public Train(int trainId, int trainSpeed, TSimInterface tsi, List<Path> pathList) {
    this.trainId = trainId;
    this.trainSpeed = trainSpeed;
    this.tsi = tsi;
    this.pathList = pathList;
    TowardsStation2 = trainId == 1;
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