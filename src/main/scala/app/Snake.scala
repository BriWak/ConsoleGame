package app

import app.ConsoleGame.isGameOn

import scala.collection.mutable.ArrayBuffer

case class Board(width: Int, height: Int)

class Snake(name: String, colour: String = "blue", gameArea: Board, private var index: Int, private var lastDirection: String = "") {

  var tail: ArrayBuffer[Int] = ArrayBuffer(index)
  private var isAlive = true

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
    this.isAlive
  }

  def getIndex: Int = {
    this.index
  }

  def moveLeft = {
    if ((index - 2) / gameArea.width == index / gameArea.width && index - 2 >= 0) {
      index = index - 2
      tail = tail :+ index
      setLastDirection("left")
    } else {
      isGameOn = false
      isAlive = false
    }
  }

  def moveRight = {
    if ((index + 2) % gameArea.width != 0) {
      index = index + 2
      tail = tail :+ index
      setLastDirection("right")
    } else {
      isGameOn = false
      isAlive = false
    }
  }

  def moveUp = {
    if ((index - gameArea.width) >= 0) {
      index = index - gameArea.width
      tail = tail :+ index
      setLastDirection("up")
    } else {
      isGameOn = false
      isAlive = false
    }
  }

  def moveDown = {
    if ((index + gameArea.width) < gameArea.width*gameArea.height) {
      index = index + gameArea.width
      tail = tail :+ index
      setLastDirection("down")
    } else {
      isGameOn = false
      isAlive = false
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