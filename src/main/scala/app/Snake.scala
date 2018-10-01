package app

import scala.collection.mutable.ArrayBuffer

case class Board(width: Int, height: Int)

class Snake(name: String, colour: String = "blue", gameArea: Board, var index: Int, private var lastDirection: String = "") {

  var tail: ArrayBuffer[Int] = ArrayBuffer(index)

  def getName: String = {
    this.name
  }

  def getShape ={
    val consoleColor = this.colour match {
      case "red" => s"${Console.RED}"
      case "green" => s"${Console.GREEN}"
      case "blue" => s"${Console.BLUE}"
      case _ => s"${Console.YELLOW}"
    }
    consoleColor+s"■${Console.RESET}"
  }

  def isStillAlive: Boolean = {
//    this.isAlive
      (lastDirection == "left" && (index - 2) / gameArea.width == index / gameArea.width && index - 2 >= 0) ||
      (lastDirection == "right" && (index + 2) % gameArea.width != 0) ||
      (lastDirection == "up" && (index - gameArea.width) >= 0) ||
      (lastDirection == "down" && (index + gameArea.width) < gameArea.width*gameArea.height) ||
      this.tail.init.contains(this.getIndex)
  }

  def getIndex: Int = {
    this.index
  }

  def moveLeft = {
    setLastDirection("left")
    if (isStillAlive) {
      index = index - 2
      tail = tail :+ index
    }
  }

  def moveRight = {
    setLastDirection("right")
    if (isStillAlive) {
      index = index + 2
      tail = tail :+ index
    }
  }

  def moveUp = {
    setLastDirection("up")
    if (isStillAlive) {
      index = index - gameArea.width
      tail = tail :+ index
    }
  }

  def moveDown = {
    setLastDirection("down")
    if (isStillAlive) {
      index = index + gameArea.width
      tail = tail :+ index
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

}