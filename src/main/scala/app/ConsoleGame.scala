package app

import jline.console.Operation

object ConsoleGame extends App {
  var isGameOn = true
  val board = Board(40, 20)
  val ship = new Ship("dot","blue", board, 420)
  val keyPresses = CaptureKeyPresses(isGameOn)

  var tick: Int = 0

  // inside the main thread
  while (isGameOn) {
    if (!keyPresses.isEmpty) {
        handleKeypress(ship, keyPresses.poll)
      } else {
      ship.keepMoving
    }
    printBoard(ship, board.width,board.height)
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

  def printBoard(ship: Ship, width: Int, height: Int): Unit = {
    if (ship.tail.init.contains(ship.getIndex)) isGameOn = false
    clear()
    println(s"${Console.GREEN} Press 'q' to quit${Console.RESET}")
    println(" ┏━" + "━" * width + "━┓")

    val matrix = List.range(0, width*height).map(index => if(ship.tail.contains(index)) ship.getShape else " ").grouped(width)

    matrix.foreach(row => {
      print(" ┃ ")
      row.foreach(cell => print(cell))
      println(" ┃")
    })

    println(" ┗━" + "━" * width + "━┛")
  }

}
