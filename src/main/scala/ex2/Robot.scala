package ex2

import scala.util.Random

type Position = (Int, Int)
enum Direction:
  case North, East, South, West
  def turnRight: Direction = this match
    case Direction.North => Direction.East
    case Direction.East => Direction.South
    case Direction.South => Direction.West
    case Direction.West => Direction.North

  def turnLeft: Direction = this match
    case Direction.North => Direction.West
    case Direction.West => Direction.South
    case Direction.South => Direction.East
    case Direction.East => Direction.North

trait Robot:
  def position: Position
  def direction: Direction
  def turn(dir: Direction): Unit
  def act(): Unit

class SimpleRobot(var position: Position, var direction: Direction) extends Robot:
  def turn(dir: Direction): Unit = direction = dir
  def act(): Unit = position = direction match
    case Direction.North => (position._1, position._2 + 1)
    case Direction.East => (position._1 + 1, position._2)
    case Direction.South => (position._1, position._2 - 1)
    case Direction.West => (position._1 - 1, position._2)

  override def toString: String = s"robot at $position facing $direction"

class DumbRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, act}
  override def turn(dir: Direction): Unit = {}
  override def toString: String = s"${robot.toString} (Dump)"

class LoggingRobot(val robot: Robot) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    robot.act()
    println(robot.toString)

class RobotWithBattery(val robot: LoggingRobot) extends Robot:
  var battery: Int = 100
  val actConsumption: Int = 50
  export robot.{position, direction, turn}
  override def act(): Unit =
    if battery > 0 && battery >= actConsumption
    then {robot.act(); battery = battery - actConsumption}
    else println("Action failed! Not enough battery.")
  override def toString: String = s"${robot.toString} with battery at $battery%"

class RobotCanFail(val robot: LoggingRobot, val prob: Int) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    if !List.from(1 to prob).contains(Random.nextInt() % 10 + 1) then robot.act() else println("Action failed")
  override def toString: String = s"${robot.toString} with $prob% to fail"

class RobotRepeated(val robot: LoggingRobot, val reps: Int) extends Robot:
  export robot.{position, direction, turn}
  override def act(): Unit =
    @annotation.tailrec
    def repeatAction(repsToDo: Int): Unit = repsToDo match
      case n => if n > 0 then {robot.act(); repeatAction(n - 1)}
    repeatAction(reps)
  override def toString: String = s"${robot.toString} with $reps repetition for every action"

@main def testRobot(): Unit =
  println("LOGGIN ROBOT")
  val robot = LoggingRobot(SimpleRobot((0, 0), Direction.North))
  robot.act() // robot at (0, 1) facing North
  robot.turn(robot.direction.turnRight) // robot at (0, 1) facing East
  robot.act() // robot at (1, 1) facing East
  robot.act() // robot at (2, 1) facing East
  println()

  println("BATTERY ROBOT")
  val batteryRobot = RobotWithBattery(LoggingRobot(SimpleRobot((0, 0), Direction.North)))
  println(batteryRobot)
  batteryRobot.act() // robot at (0, 1) facing North
  println(batteryRobot)
  batteryRobot.act() // robot at (0, 2) facing North
  println(batteryRobot)
  batteryRobot.act() // robot still at (0, 2) facing North
  println(batteryRobot)
  println()

  println("ROBOT CAN FAIL")
  val failingRobot = RobotCanFail(LoggingRobot(SimpleRobot((0, 0), Direction.North)), 7)
  println(failingRobot)
  failingRobot.act() // robot should be at (0, 1) facing North
  failingRobot.act() // robot should be at (0, 2) facing North
  failingRobot.act() // robot should be at (0, 3) facing North
  println()

  println("ROBOT REPEATED")
  val repeatedRobot = RobotRepeated(LoggingRobot(SimpleRobot((0, 0), Direction.North)), 5)
  println(repeatedRobot)
  repeatedRobot.act() // robot should be at (0, 5) facing North
  repeatedRobot.act() // robot should be at (0, 10) facing North
  repeatedRobot.act() // robot should be at (0, 15) facing North
  println()