package app

class Ship(shape: String, colour: String = s"${Console.RED}", private var index: Int) {

  def eraseScreen: Unit = {
    print("\033[H\033[2J")
  }

  var canMove = true

  def getIndex: Int = {
    this.index
  }

  def moveLeft = {
    if (canMove && (index - 1) / 10 == index / 10 && index - 1 > 0) {
      index = index - 1
    }
  }

  def moveRight = {
    if (canMove && (index + 1) % 10 != 0) {
      index = index + 1
    }
  }


  def stopMove = {
    canMove = false
  }

}