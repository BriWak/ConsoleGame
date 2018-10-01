package app

import app.ConsoleGame.player1
import app.ConsoleGame.player2

object GameOn {
  def apply(): Boolean = {

    if ((!player1.isStillAlive && !player2.isStillAlive) ||
      (player2.tail.init.contains(player1.getIndex) && player1.tail.init.contains(player2.getIndex))) {
      println("\nGame Over! You both lose!\n")
      false
    }
    else if (!player1.isStillAlive || player2.tail.init.contains(player1.getIndex)) {
      println(s"\n${player2.getName} wins!\n")
      false
    }
    else if (!player2.isStillAlive || player1.tail.init.contains(player2.getIndex)) {
      println(s"\n${player1.getName} wins!\n")
      false
    }
    else {
      true
    }
  }

}
