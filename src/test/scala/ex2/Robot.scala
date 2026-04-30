package ex2

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class RobotSpec extends AnyFlatSpec with Matchers:
  "A SimpleRobot" should "turn correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.turn(Direction.East)
    robot.direction should be(Direction.East)

    robot.turn(Direction.South)
    robot.direction should be(Direction.South)

    robot.turn(Direction.West)
    robot.direction should be(Direction.West)

    robot.turn(Direction.North)
    robot.direction should be(Direction.North)

  it should "act correctly" in:
    val robot = new SimpleRobot((0, 0), Direction.North)

    robot.act()
    robot.position should be((0, 1))

    robot.turn(Direction.East)
    robot.act()
    robot.position should be((1, 1))

    robot.turn(Direction.South)
    robot.act()
    robot.position should be((1, 0))

    robot.turn(Direction.West)
    robot.act()
    robot.position should be((0, 0))

  "A RobotWithBattery" should "execute action if battery is enough" in:
    val batteryRobot = RobotWithBattery(LoggingRobot(SimpleRobot((0, 0), Direction.North)))
    var previousBattery = batteryRobot.battery

    batteryRobot.act()
    batteryRobot.position should be((0,1))
    assert(batteryRobot.battery < previousBattery)
    previousBattery = batteryRobot.battery

    batteryRobot.act()
    batteryRobot.position should be((0, 2))
    assert(batteryRobot.battery < previousBattery)
    previousBattery = batteryRobot.battery

    batteryRobot.act()
    batteryRobot.position should be((0, 2))
    assert(batteryRobot.battery == previousBattery)

  "A RobotCanFail with prob = 0" should "always execute action" in :
    val robotCanFail = RobotCanFail(LoggingRobot(SimpleRobot((0, 0), Direction.North)), 0)
    val initialPosition = robotCanFail.position

    robotCanFail.act()
    robotCanFail.position should not be(initialPosition)

  "A RobotCanFail with prob = 100" should "never execute action" in :
    val robotCanFail = RobotCanFail(LoggingRobot(SimpleRobot((0, 0), Direction.North)), 100)
    val initialPosition = robotCanFail.position

    robotCanFail.act()
    robotCanFail.position should be(initialPosition)

  "A RobotCanFail with prob less than 0 or greater than 100" should "throw an IllegalArgumentException" in :
    assertThrows[IllegalArgumentException] {RobotCanFail(LoggingRobot(SimpleRobot((0, 0), Direction.North)), -100)}

  "A RobotRepeated with reps == 5" should "always execute action 5 times" in :
    val repeatedRobot = RobotRepeated(LoggingRobot(SimpleRobot((0, 0), Direction.North)), 5)
    var initialPosition = repeatedRobot.position

    repeatedRobot.act()
    repeatedRobot.position should be((0, 5))

    repeatedRobot.act()
    repeatedRobot.position should be((0, 10))

    repeatedRobot.act()
    repeatedRobot.position should be((0, 15))