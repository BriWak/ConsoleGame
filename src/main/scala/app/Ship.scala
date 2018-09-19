package app

import scala.collection.mutable.ArrayBuffer

case class Board(width: Int, height: Int)

class Ship(shape: String, colour: String = "blue", flightLimits: Board, private var index: Int) {

  private var canMove = true
  private var lastDirection: String = ""
  var tail: ArrayBuffer[Int] = ArrayBuffer(index)


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
    if (canMove && (index - 2) / flightLimits.width == index / flightLimits.width && index - 2 >= 0) {
      index = index - 2
      tail = tail :+ index
      setLastDirection("left")
    }
  }

  def moveRight = {
    if (canMove && (index + 2) % flightLimits.width != 0) {
      index = index + 2
      tail = tail :+ index
      setLastDirection("right")
    }
  }

  def moveUp = {
    if (canMove && (index - flightLimits.width) >= 0) {
      index = index - flightLimits.width
      setLastDirection("up")
      tail = tail :+ index
    }
  }

  def moveDown = {
    if (canMove && (index + flightLimits.width) < flightLimits.width*flightLimits.height) {
      index = index + flightLimits.width
      setLastDirection("down")
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

  def stopMove = {
    canMove = false
  }

}