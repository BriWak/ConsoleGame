package app

class Ship(shape: String, colour: String = "blue", bW: Int, bH: Int, private var index: Int, private var lastDirection: String = "") {

  def eraseScreen: Unit = {
    print("\033[H\033[2J")
  }

  var canMove = true

  def getShape ={
    val consoleColor = this.colour match {
      case "red" => s"${Console.RED}"
      case "green" => s"${Console.GREEN}"
      case "blue" => s"${Console.BLUE}"
      case _ => s"${Console.YELLOW}"
    }

    consoleColor+s"■${Console.RESET}"
  }
  def getBoardWidth: Int = {
    this.bW
  }

  def getBoardHeight: Int = {
    this.bH
  }

  def getIndex: Int = {
    this.index
  }

  def moveLeft = {
    if (canMove && (index - 2) / bW == index / bW && index - 2 >= 0) {
      index = index - 2
    }
  }

  def moveRight = {
    if (canMove && (index + 2) % bW != 0) {
      index = index + 2
    }
  }

  def moveUp = {
    if (canMove && (index - bW) >= 0) {
      index = index - bW
    }
  }

  def moveDown = {
    if (canMove && (index + bW) < bW*bH) {
      index = index + bW
    }
  }

  def setLastDirection(direction: String) = {
  direction match {
    case "left" => this.lastDirection = direction
    case "right" => this.lastDirection = direction
    case "up" => this.lastDirection = direction
    case "down" => this.lastDirection = direction
    case _ => this.lastDirection = ""
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