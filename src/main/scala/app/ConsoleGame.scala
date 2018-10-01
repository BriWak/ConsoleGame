package app

import jline.console.Operation

object ConsoleGame extends App {
  val board = Board(40, 20)
  val player1 = new Snake("Player 1","blue", board, board.height*board.width-board.width, "right")
  val player2 = new Snake("Player 2","yellow", board, board.width-2, "left")
  val keyPresses = CaptureKeyPresses(GameOn())

  // inside the main thread
  while (GameOn()) {
    if (!keyPresses.isEmpty) {
        handleKeypress(player1, player2, keyPresses.poll)
      } else {
      player1.keepMoving
      player2.keepMoving
    }
    printBoard(player1, player2, board.width,board.height)
    Thread.sleep(100)
  }

  def clear(): Unit ={
    print("\033[H\033[2J")
  }

  def handleKeypress(player1: Snake, player2: Snake, key: Either[Operation, String]) =
    key match {
      case Right("p") | Left(Operation.VI_EOF_MAYBE) =>
        //isGameOn = false
      // Left arrow
      case Left(Operation.BACKWARD_CHAR) =>
        player1.moveLeft
      // Right arrow
      case Left(Operation.FORWARD_CHAR) =>
        player1.moveRight
      // Down arrow
      case Left(Operation.NEXT_HISTORY) =>
        player1.moveDown
      // Up arrow
      case Right("w") =>
        player2.moveUp
      case Right("a") =>
        player2.moveLeft
      // Right arrow
      case Right("d") =>
        player2.moveRight
      // Down arrow
      case Right("s") =>
        player2.moveDown
      // Up arrow
      case Left(Operation.PREVIOUS_HISTORY) =>
        player1.moveUp
      case _ =>
        // println(k)
    }

  def printBoard(player1: Snake, player2: Snake, width: Int, height: Int): Unit = {

    clear()
    PrintTitle("tron", "blue")
    println(" ┏━" + "━" * width + "━┓")

    val matrix = List.range(0, width*height).map(index =>
      if(player1.tail.contains(index)) player1.getShape
      else if (player2.tail.contains(index)) player2.getShape
      else " ").grouped(width)

    matrix.foreach(row => {
      print(" ┃ ")
      row.foreach(cell => print(cell))
      println(" ┃")
    })

    println(" ┗━" + "━" * width + "━┛")
    println(s"${Console.GREEN} Press 'p' to quit${Console.RESET}")

    GameOn()
  }



}
