package app

case class Board(width: Int, height: Int)

class Ship(shape: String, colour: String = "blue", board: Board, private var index: Int) {

  private var canMove = true
  private var lastDirection: String = ""

  def getShape ={
    val consoleColor = this.colour match {
      case "red" => s"${Console.RED}"
      case "green" => s"${Console.GREEN}"
      case "blue" => s"${Console.BLUE}"
      case _ => s"${Console.YELLOW}"
    }

    consoleColor+s"■${Console.RESET}"
  }

  def getIndex: Int = {
    this.index
  }

  def moveLeft = {
    if (canMove && (index - 2) / board.width == index / board.width && index - 2 >= 0) {
      index = index - 2
      setLastDirection("left")
    }
  }

  def moveRight = {
    if (canMove && (index + 2) % board.width != 0) {
      index = index + 2
      setLastDirection("right")
    }
  }

  def moveUp = {
    if (canMove && (index - board.width) >= 0) {
      index = index - board.width
      setLastDirection("up")
    }
  }

  def moveDown = {
    if (canMove && (index + board.width) < board.width*board.height) {
      index = index + board.width
      setLastDirection("down")
    }
  }

  def setLastDirection(direction: String) = {
  direction match {
    case "left" => lastDirection = direction
    case "right" => lastDirection = direction
    case "up" => lastDirection = direction
    case "down" => lastDirection = direction
    case _ => lastDirection = ""
    }
  }

  def keepMoving = {
    this.lastDirection match {
      case "left" => this.moveLeft
      case "right" => this.moveRight
      case "up" => this.moveUp
      case "down" => this.moveDown
      case _ =>
    }
  }

  def stopMove = {
    canMove = false
  }

}