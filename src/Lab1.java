import TSim.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Lab1 {
  List<Path> pathList;
  Crossing crossing;

  public Lab1(int speed1, int speed2) {
    TSimInterface tsi = TSimInterface.getInstance();
    // Adds all separate paths to the lists
    pathList = new ArrayList<>();
    Point switch1 = new Point(17, 7);
    Point switch2 = new Point(15, 9);
    Point switch3 = new Point(4, 9);
    Point switch4 = new Point(3, 11);
    pathList.add(new Path(15, 3, 15, 7, 1, new Point[] { switch1, null }));
    pathList.add(new Path(15, 5, 15, 8, 2, new Point[] { switch1, null }));
    pathList.add(new Path(19, 7, 17, 9, 3, new Point[] { switch2, switch1 }));
    pathList.add(new Path(13, 9, 6, 9, 4, new Point[] { switch3, switch2 }));
    pathList.add(new Path(13, 10, 6, 10, 5, new Point[] { switch3, switch2 }));
    pathList.add(new Path(2, 9, 1, 11, 6, new Point[] { switch4, switch3 }));
    pathList.add(new Path(5, 11, 15, 11, 7, new Point[] { null, switch4 }));
    pathList.add(new Path(4, 13, 15, 13, 8, new Point[] { null, switch4 }));

    crossing = new Crossing(6, 6, 11, 7, 8, 5, 10, 8);

    Train train1 = new Train(1, speed1, tsi, pathList, crossing);
    Train train2 = new Train(2, speed2, tsi, pathList, crossing);

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
  int currentPathIndex = 0; // 0 = null
  int lastPathIndex = 0;
  boolean TowardsStation2;
  List<Path> pathList;
  Crossing crossing;
  TSimInterface tsi;
  // Semaphore paths = new Semaphore(1);

  @Override
  public void run() {
    try {
      tsi.setSpeed(trainId, trainSpeed);
      for (Path currentPath : pathList) {
        if (currentPath.index == currentPathIndex) {
          currentPath.busy.acquire();
        }
      }
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

    // Only act on sensorEvent ACTIVE
    if (sensorE.getStatus() == sensorE.INACTIVE) {
      return;
    }
    Point currentPos = new Point(sensorE.getXpos(), sensorE.getYpos());

    // Entering the crossing
    if (crossing.pos1[TowardsStation2 ? 0 : 1].equals(currentPos)
        || crossing.pos2[TowardsStation2 ? 0 : 1].equals(currentPos)) {
      tsi.setSpeed(trainId, 0);
      crossing.busy.acquire();
      tsi.setSpeed(trainId, trainSpeed);
    }

    // Leaving crossing
    else if (TowardsStation2 && crossing.pos1[1].equals(currentPos) ||
        TowardsStation2 && crossing.pos2[1].equals(currentPos) ||
        !TowardsStation2 && crossing.pos1[0].equals(currentPos) ||
        !TowardsStation2 && crossing.pos2[0].equals(currentPos)) {
      crossing.busy.release();
    }

    Path currentPath = null;
    lastPathIndex = currentPathIndex;
    breakLoop: {
      for (Path pa : pathList) {
        for (Point po : pa.pos) {
          // Checks if correct currentPath
          if ((po.equals(currentPos))) {
            currentPath = pa;
            break breakLoop;
          }
        }
      }
    }
    if (currentPath == null) {
      return;
    }
    // Setup
    currentPathIndex = currentPath.index;
    int nextIndex = getNextDesiredPath();
    Path nextPath = pathList.get(nextIndex - 1);
    Semaphore nextSemaphore = nextPath.busy;
    // Exit trigger
    if (currentPath.pos[TowardsStation2 ? 1 : 0].equals(currentPos)) {
      // If reached opposite station
      if ((!TowardsStation2 && (currentPathIndex == 1 || currentPathIndex == 2))
          || (TowardsStation2 && (currentPathIndex == 7 || currentPathIndex == 8))) {
        sleepAtStation();
        return;
      }

      // If currentPath is single file
      if (nextIndex == 3 || nextIndex == 6) {
        tsi.setSpeed(trainId, 0);
        nextSemaphore.acquire();
      }

      // If currentPath has multiple choice
      else {
        tsi.setSpeed(trainId, 0);
        if (!nextSemaphore.tryAcquire(5, TimeUnit.MILLISECONDS)) {
          pathList.get(nextIndex).busy.acquire();
          nextIndex++;
        }
      }

      setSwitch(nextIndex);
      tsi.setSpeed(trainId, trainSpeed);
    }

    // Enter trigger
    else {
      pathList.get(lastPathIndex - 1).busy.release();
    }
  }

  private void setSwitch(int nextPathIndex) throws CommandException {
    Path currentPath = pathList.get(currentPathIndex - 1);
    int switchDir = tsi.SWITCH_RIGHT;
    switch (currentPathIndex) {
      case 2:
      case 4:
        switchDir = (TowardsStation2 ? tsi.SWITCH_LEFT : switchDir);
        break;
      case 5:
      case 7:
        switchDir = (!TowardsStation2 ? tsi.SWITCH_LEFT : switchDir);
        break;
      case 3:
      case 6:
        switchDir = (nextPathIndex == 2 || nextPathIndex == 7
            || (nextPathIndex == (TowardsStation2 ? 5 : 4)) ? tsi.SWITCH_LEFT
                : switchDir);
        break;
      default:
        break;
    }
    Point sw = currentPath.attachedSwitches[TowardsStation2 ? 0 : 1];
    tsi.setSwitch(sw.x, sw.y, switchDir);

  }

  private int getNextDesiredPath() {
    int nextPathIndex = currentPathIndex;
    if (TowardsStation2) {
      switch (currentPathIndex) {
        case 1:
        case 4:
          nextPathIndex = currentPathIndex + 2;
          break;
        case 2:
        case 3:
        case 5:
        case 6:
          nextPathIndex = currentPathIndex + 1;
          break;
        default:
          break;
      }
    }

    else {
      switch (currentPathIndex) {
        case 3:
        case 5:
        case 6:
        case 8:
          nextPathIndex = currentPathIndex - 2;
          break;
        case 4:
        case 7:
          nextPathIndex = currentPathIndex - 1;
          break;
        default:
          break;
      }
    }
    return nextPathIndex;
  }

  private void sleepAtStation() throws CommandException, InterruptedException {
    tsi.setSpeed(trainId, 0);
    trainSpeed *= -1;
    Thread.sleep(1000 + 20 * Math.abs(trainSpeed));
    TowardsStation2 = !TowardsStation2;
    tsi.setSpeed(trainId, trainSpeed);
  }

  public Train(int trainId, int trainSpeed, TSimInterface tsi, List<Path> pathList, Crossing crossing) {
    this.trainId = trainId;
    this.trainSpeed = trainSpeed;
    this.tsi = tsi;
    this.pathList = pathList;
    this.crossing = crossing;
    TowardsStation2 = (trainId == 1);
    currentPathIndex = (TowardsStation2 ? 1 : 7);
  }
}

class Crossing {
  public Point[] pos1;
  public Point[] pos2;
  public Semaphore busy;

  public Crossing(int x11, int y11, int x12, int y12, int x21, int y21, int x22, int y22) {
    pos1 = new Point[] { new Point(x11, y11), new Point(x12, y12) };
    pos2 = new Point[] { new Point(x21, y21), new Point(x22, y22) };
    busy = new Semaphore(1);
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

  public boolean equals(int x, int y) {
    return (x == this.x && y == this.y);
  }

  public boolean equals(Point p1) {
    return equals(p1.getX(), p1.getY());
  }
}