package ex3

object Solitaire extends App:
  def render(solution: Seq[(Int, Int)], width: Int, height: Int): String =
    val reversed = solution.reverse
    val rows =
      for y <- 0 until height
          row = for x <- 0 until width
          number = reversed.indexOf((x, y)) + 1
          yield if number > 0 then "%-2d ".format(number) else "X  "
      yield row.mkString
    rows.mkString("\n")

  def placeMarks(width: Int, height: Int): List[List[(Int,Int)]] =
    def place(solution: List[(Int,Int)]): List[List[(Int,Int)]] =
      if solution.size == width * height
        then List(solution)
        else
          for
            value <- possibleMoves(solution.last._1, solution.last._2, solution, width, height)
            if isValid(value, solution, width, height)
            result <- place(solution :+ value)
          yield
            result
    place(List((width/2, height/2)))

  def isValid(pos: (Int,Int), sol: List[(Int,Int)], width: Int, height: Int): Boolean =
    if sol.isEmpty
      then true
    else
      val x = sol.last._1
      val y = sol.last._2
      (math.abs(x - pos._1) == 2 && math.abs(y - pos._2) == 2) ||
      (math.abs(x - pos._1) == 3 && math.abs(y - pos._2) == 0) ||
      (math.abs(x - pos._1) == 0 && math.abs(y - pos._2) == 3)

  def possibleMoves(x: Int, y: Int, sol: List[(Int,Int)], width: Int, height: Int): List[(Int,Int)] =
    List(
      (x+3, y), (x-3, y),
      (x, y+3), (x, y-3),
      (x+2, y+2), (x-2, y-2),
      (x+2, y-2), (x-2, y+2)
    ).filter((x,y) => (x >= 0 && x < width) && (y >= 0 && y < height) && !sol.contains((x,y)))

  val w: Int = 5
  val h: Int = 7
  placeMarks(w,h).foreach(solution => println(render(solution,w,h) + "\n"))