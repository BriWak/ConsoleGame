package app

object PrintTitle {
  def apply(title: String, colour: String) = {

    val x = title.toLowerCase.map {
      case 'a' =>
        Seq(" ■■■   ",
            "■   ■  ",
            "■■■■■  ",
            "■   ■  ",
            "■   ■  ")

      case 'b' =>
        Seq("■■■■   ",
            "■   ■  ",
            "■■■■   ",
            "■   ■  ",
            "■■■■   ")

      case 'c' =>
        Seq(" ■■■   ",
            "■   ■  ",
            "■      ",
            "■   ■  ",
            " ■■■   ")

      case 'd' =>
        Seq("■■■■   ",
            "■   ■  ",
            "■   ■  ",
            "■   ■  ",
            "■■■■   ")

      case 'e' =>
        Seq("■■■■■  ",
            "■      ",
            "■■■■   ",
            "■      ",
            "■■■■■  ")

      case 'f' =>
        Seq("■■■■■  ",
            "■      ",
            "■■■    ",
            "■      ",
            "■      ")

      case 'g' =>
        Seq(" ■■■   ",
            "■      ",
            "■  ■■■ ",
            "■   ■  ",
            " ■■■   ")

      case 'h' =>
        Seq("■   ■  ",
            "■   ■  ",
            "■■■■■  ",
            "■   ■  ",
            "■   ■  ")

      case 'i' =>
        Seq("■■■■■  ",
            "  ■    ",
            "  ■    ",
            "  ■    ",
            "■■■■■  ")

      case 'j' =>
        Seq(" ■■■■■ ",
            "   ■   ",
            "   ■   ",
            "■  ■   ",
            " ■■    ")

      case 'k' =>
        Seq("■   ■  ",
            "■  ■   ",
            "■■■    ",
            "■  ■   ",
            "■   ■  ")

      case 'l' =>
        Seq("■      ",
            "■      ",
            "■      ",
            "■      ",
            "■■■■■  ")

      case 'm' =>
        Seq(" ■■ ■■   ",
            "■  ■  ■  ",
            "■  ■  ■  ",
            "■  ■  ■  ",
            "■  ■  ■  ")

      case 'n' =>
        Seq("■   ■  ",
            "■■  ■  ",
            "■ ■ ■  ",
            "■  ■■  ",
            "■   ■  ")

      case 'o' =>
        Seq(" ■■■   ",
            "■   ■  ",
            "■   ■  ",
            "■   ■  ",
            " ■■■   ")

      case 'p' =>
        Seq("■■■■   ",
            "■   ■  ",
            "■■■■   ",
            "■      ",
            "■      ")

      case 'q' =>
        Seq(" ■■■   ",
            "■   ■  ",
            "■ ■ ■  ",
            "■   ■  ",
            " ■■■ ■ ")

      case 'r' =>
        Seq("■■■■   ",
            "■   ■  ",
            "■■■■   ",
            "■   ■  ",
            "■   ■  ")

      case 's' =>
        Seq(" ■■■■  ",
            "■      ",
            " ■■■■  ",
            "     ■ ",
            " ■■■■  ")

      case 't' =>
        Seq("■■■■■  ",
            "  ■    ",
            "  ■    ",
            "  ■    ",
            "  ■    ")

      case 'u' =>
        Seq("■   ■  ",
            "■   ■  ",
            "■   ■  ",
            "■   ■  ",
            " ■■■   ")

      case 'v' =>
        Seq("■   ■  ",
            "■   ■  ",
            "■   ■  ",
            " ■ ■   ",
            "  ■    ")

      case 'w' =>
        Seq("■  ■  ■ ",
            "■  ■  ■ ",
            "■  ■  ■ ",
            "■  ■  ■ ",
            " ■■ ■■  ")

      case 'x' =>
        Seq("■   ■  ",
            " ■ ■   ",
            "  ■    ",
            " ■ ■   ",
            "■   ■  ")

      case 'y' =>
        Seq("■   ■  ",
            " ■ ■   ",
            "  ■    ",
            "  ■    ",
            "  ■    ")

      case 'z' =>
        Seq("■■■■■  ",
            "   ■   ",
            "  ■    ",
            " ■     ",
            "■■■■■  ")
    }

    val characters = x.flatMap(_.zipWithIndex).groupBy(_._2)
    val consoleColor = colour.toLowerCase match {
      case "red" => s"${Console.RED}"
      case "green" => s"${Console.GREEN}"
      case "blue" => s"${Console.BLUE}"
      case _ => s"${Console.YELLOW}"
    }
    println(s"$consoleColor")
    println(s" ${characters(0).map(x => x._1).mkString}")
    println(s" ${characters(1).map(x => x._1).mkString}")
    println(s" ${characters(2).map(x => x._1).mkString}")
    println(s" ${characters(3).map(x => x._1).mkString}")
    println(s" ${characters(4).map(x => x._1).mkString}")
    println(s"${Console.RESET}")

  }
}