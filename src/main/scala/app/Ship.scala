package app

class Ship(shape: String, colour: String = "blue", bW: Int, bH: Int, private var index: Int) {

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
    if (canMove && (index - 1) / bW == index / bW && index - 1 >= 0) {
      index = index - 1
    }
  }

  def moveRight = {
    if (canMove && (index + 1) % bW != 0) {
      index = index + 1
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

  def stopMove = {
    canMove = false
  }

}