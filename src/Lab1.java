import TSim.*;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;

import javax.sound.sampled.SourceDataLine;
import javax.swing.text.AbstractDocument.BranchElement;

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
    pathList.add(new Path(15, 3, 15, 7, 1, new Point[] { switch1, null }));
    pathList.add(new Path(15, 5, 15, 8, 2, new Point[] { switch1, null }));
    pathList.add(new Path(19, 7, 17, 9, 3, new Point[] { switch2, switch1 }));
    pathList.add(new Path(13, 9, 6, 9, 4, new Point[] { switch3, switch2 }));
    pathList.add(new Path(13, 10, 5, 10, 5, new Point[] { switch3, switch2 }));
    pathList.add(new Path(2, 9, 1, 11, 6, new Point[] { switch4, switch3 }));
    pathList.add(new Path(5, 11, 15, 11, 7, new Point[] { null, switch4 }));
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
  int lastPath = 0;
  boolean TowardsStation2;
  List<Path> pathList;
  TSimInterface tsi;
  // Semaphore paths = new Semaphore(1);

  @Override
  public void run() {
    try {
      tsi.setSpeed(trainId, trainSpeed);
      for (Path path : pathList){
        if(path.index == currentPath){
          path.busy.acquire();
          System.out.println("startup aquasition");
        }
      }
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

    // Only act on sensorEvent ACTIVE
    if(sensorE.getStatus() == sensorE.INACTIVE){
      return;
    }
    
    for (Path path : pathList) {
      for (Point point : path.pos) {
        
        // Checks if correct path
        if (!(point.getX() == sensorE.getXpos() && point.getY() == sensorE.getYpos())) {
          continue;
        }

        //Setup
        lastPath = currentPath;
        currentPath = path.index;
        int nextIndex = getNextDesiredPath();
        final int tempIndexLast = lastPath;
        Path nextPath = null;
        for (Path poinp : pathList) {
          if (poinp.index == nextIndex) {
            nextPath = poinp;
            break;
          }
        }
        Semaphore nextSemaphore = nextPath.busy;

        // Exit trigger
        if (((TowardsStation2 && point == path.pos[1]) || (!TowardsStation2 && point == path.pos[0]))) {

          // If reached opposite station
          if((!TowardsStation2 && (currentPath == 1 || currentPath == 2)) || (TowardsStation2 && (currentPath == 7 || currentPath == 8))){
            sleepAtStation();
            break;
          }

          // If path is single file
          if (nextIndex == 3 || nextIndex == 6){
            System.out.println("Entering new land " + trainId);
            System.out.println("Current: " + currentPath + " next: " + nextIndex);
            tsi.setSpeed(trainId, 0);
            nextSemaphore.acquire();
            System.out.println("Train " + trainId + " aquired semaphore: " + nextIndex);
          }

          // If path has multiple choice
          else{
            tsi.setSpeed(trainId, 0);
            if (!nextSemaphore.tryAcquire(5, TimeUnit.MILLISECONDS)) {
              for (Path p : pathList){
                if (p.index == nextIndex + 1){
                  nextIndex = p.index;
                  p.busy.acquire();
                  System.out.println("Train " + trainId + " aquired semaphore: " + nextIndex + " and a took detour");
                  break;
                }
              }
            }
            else{
              System.out.println("Train " + trainId + " aquired semaphore: " + nextIndex);
            }
          }

          setSwitch(nextIndex);
          tsi.setSpeed(trainId, trainSpeed);
          System.out.println("Train + " + trainId + " current is now: " + nextIndex + " in stead of: " + currentPath);
        }

        //Enter trigger
        else{
          System.out.println("Train " + trainId + " trying to release semaphore: " + lastPath);

          ((Path) pathList.stream().filter(p -> (p.index == tempIndexLast)).toArray()[0]).busy.release();

          System.out.println("Succes! " + lastPath);

        }
      }
    }
  }


  private void setSwitch(int nextPath) throws CommandException {
    System.out.println("Reached setSwitch\n\t NextPath: " + nextPath + "\n\tCurrent Path: " + currentPath);
    Path path = (Path) (pathList.stream().filter(c -> (c.index == currentPath))).toArray()[0];
    int switchDir = 0;
    if (TowardsStation2) {
      switch (currentPath) {
        case 1:
        case 5:
          switchDir = tsi.SWITCH_RIGHT;
          break;
        case 2:
        case 4:
          switchDir = tsi.SWITCH_LEFT;
          break;
        case 3:
        case 6:
          switchDir = (nextPath == 4 || nextPath == 8) ? tsi.SWITCH_RIGHT : tsi.SWITCH_LEFT;
          break;
        case 7:
        case 8:
        default:
          break;
      }
    } else {
      switch (currentPath) {
        case 3:
        case 6:
          switchDir = (nextPath == 2 || nextPath == 4) ? tsi.SWITCH_LEFT : tsi.SWITCH_RIGHT;
          break;
        case 5:
        case 7:
          switchDir = tsi.SWITCH_LEFT;
          break;
        case 4:
        case 8:
          switchDir = tsi.SWITCH_RIGHT;
          break;
        case 1:
        case 2:
        default:
          break;
      }
    }
    Point sw = path.attachedSwitches[TowardsStation2 ? 0 : 1];
    tsi.setSwitch(sw.x, sw.y, switchDir);
    System.out.println("Swapped switch " + sw.x + " " + sw.y);

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
    TowardsStation2 = (trainId == 1);
    currentPath = (TowardsStation2 ? 1 : 7);
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