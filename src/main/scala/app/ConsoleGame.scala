package app

import java.util.concurrent.ArrayBlockingQueue

import jline.console.{ConsoleReader, KeyMap, Operation}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{ExecutionContext, Future, blocking}

case class GameState(pos: (Int, Int))

object ConsoleGame extends App {
  val reader = new ConsoleReader()
  val isGameOn = true
  val keyPressses = new ArrayBlockingQueue[Either[Operation, String]](128)
  val ship = new Ship("dot",s"${Console.RED}", 5)
  printBoard(ship, 10,20)

  // inside a background thread
  val inputHandling = Future {
    val km = KeyMap.keyMaps().get("vi-insert")
    while (isGameOn) {
      blocking {
        val c = reader.readBinding(km)
        val k: Either[Operation, String] =
          if (c == Operation.SELF_INSERT) Right(reader.getLastBinding)
          else Left(c match { case op: Operation => op })
        keyPressses.add(k)
      }
    }
  }

  var tick: Int = 0

  // inside the main thread
  while (isGameOn) {
    while (!keyPressses.isEmpty) {
      Option(keyPressses.poll) foreach { k =>
        handleKeypress(ship, k)
        clear()
        printBoard(ship, 10,20)
      }
    }
    tick += 1

    Thread.sleep(100)
  }

  def clear(): Unit ={
    print("\033[H\033[2J")
  }

  def handleKeypress(ship: Ship, k: Either[Operation, String]) =
    k match {
      case Right("q") | Left(Operation.VI_EOF_MAYBE) =>
        !isGameOn
      // Left arrow
      case Left(Operation.BACKWARD_CHAR) =>
        ship.moveLeft
      // Right arrow
      case Left(Operation.FORWARD_CHAR) =>
        ship.moveRight
      // Down arrow
//      case Left(Operation.NEXT_HISTORY) =>
//        val pos0 = g.pos
//        g.copy(pos = (pos0._1, pos0._2 + 1))
//      // Up arrow
//      case Left(Operation.PREVIOUS_HISTORY) =>
//        val pos0 = g.pos
//        g.copy(pos = (pos0._1, pos0._2 - 1))
      case _ =>
        // println(k)
    }

  def printBoard(ship: Ship, bW: Int, bH: Int): Unit = {

    println("┏" + "━" * bW * 2 + "━┓")
    (1 to bH).foreach(_ => println("┃ "  + " " * bW * 2 + "┃"))
    println("┗" + "━" * bW * 2 + "━┛")
  }



}
