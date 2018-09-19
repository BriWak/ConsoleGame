package app

import java.util.concurrent.ArrayBlockingQueue

import com.sun.org.apache.xpath.internal.functions.FuncFalse
import jline.console.{ConsoleReader, KeyMap, Operation}

import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.{Future, blocking}

case class GameState(pos: (Int, Int))

object ConsoleGame extends App {
  val reader = new ConsoleReader()
  var isGameOn = true
  val keyPressses = new ArrayBlockingQueue[Either[Operation, String]](128)
  val ship = new Ship("dot",s"${Console.RED}",40,20, 0)
  printBoard(ship, ship.getBoardWidth,ship.getBoardHeight)

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
        printBoard(ship, ship.getBoardWidth,ship.getBoardHeight)
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
        isGameOn = false
      // Left arrow
      case Left(Operation.BACKWARD_CHAR) =>
        ship.moveLeft
      // Right arrow
      case Left(Operation.FORWARD_CHAR) =>
        ship.moveRight
      // Down arrow
      case Left(Operation.NEXT_HISTORY) =>
        ship.moveDown
      // Up arrow
      case Left(Operation.PREVIOUS_HISTORY) =>
        ship.moveUp
      case _ =>
        // println(k)
    }

  def printBoard(ship: Ship, bW: Int, bH: Int): Unit = {

    println("┏━" + "━" * bW + "━┓")

    val xxx = List.range(0, bW*bH).map(index => if(ship.getIndex == index) ship.getShape else " ").grouped(bW)

    xxx.foreach(row => {
      print("┃ ")
      row.foreach(cell => print(cell))
      println(" ┃")
    })
    println("┗━" + "━" * bW + "━┛")
  }



}
