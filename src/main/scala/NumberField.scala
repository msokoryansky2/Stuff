import util.{Properties, Random}

/**
  * Class representing a two-dimensional "field" of integers.
  * Each element of the inner array is a row in the field.
  * @param field represents two-dimension field of numbers.
  */
class NumberField (field: Seq[Seq[Int]]) {
  require(NumberField.isValid(field), "Invalid number field specified")
  val height: Int = field.length
  val width: Int = field.head.length
  val elSize: Int =
    field.map(row => row.max).max.toString.length +  (if (field.exists(_.exists(_ < 0))) 1 else 0)

  def isEl(x: Int, y: Int): Boolean = y >= 0 && y < height && x >= 0 && x < width

  def el(x: Int, y: Int): Int = {
    require(isEl(x, y), s"Invalid co-ordinates ($x, $y) specified")
    field(y)(x)
  }

  def evalPath(path: List[(Int, Int)]): Int = path.foldLeft(0)((e, cell) => e + el(cell._1, cell._2))

  /**
    * Best path from any point (x, y) (defaulting top left corner, aka (0, 0)) to lower right corner.
    * Correct but very inefficient.
    */
  def bestPathSlow(x: Int = 0, y: Int = 0): List[(Int, Int)] = {
    def bestPathSlowAcc(x: Int, y: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      val accNext = (x, y) :: acc
      (x, y) match {
        case (right, bottom) if right == width - 1 && bottom == height - 1 => accNext
        case (right, other) if right == width - 1 => bestPathSlowAcc(x, y + 1, accNext)
        case (other, bottom) if bottom == height - 1 => bestPathSlowAcc(x + 1, y, accNext)
        case _ =>
          val overPath = bestPathSlowAcc(x + 1, y, accNext)
          val downPath = bestPathSlowAcc(x, y + 1, accNext)
          if (evalPath(overPath) > evalPath(downPath)) overPath else downPath
      }
    }
    bestPathSlowAcc(0, 0, List()).reverse
  }

  def bestPath(x: Int = 0, y: Int = 0): List[(Int, Int)] = allBestPaths(x, y)

  /**
    * All best paths from any point (x, y) to lower right corner
    */
  lazy val allBestPaths: Map[(Int, Int), List[(Int, Int)]] = {
    def allBestPathsAcc(x: Int, y: Int, acc:Map[(Int, Int), List[(Int, Int)]]):Map[(Int, Int), List[(Int, Int)]] = {
      if (acc.contains((x, y))) acc
      else (x, y) match {
        case (right, bottom) if right == width - 1 && bottom == height - 1 => acc ++ Map((x, y) -> List((x, y)))
        case _ =>
          val acc2 = if (isEl(x, y + 1)) allBestPathsAcc(x, y + 1, acc) else acc
          val acc3 = if (isEl(x + 1, y)) allBestPathsAcc(x + 1, y, acc2) else acc2
          acc3 ++ Map((x, y) -> ((x, y) ::
            (if (isEl(x + 1, y) && isEl(x, y + 1))
              if (evalPath(acc3(x + 1, y)) > evalPath(acc3(x, y + 1))) acc3(x + 1, y) else acc3(x, y + 1)
            else if (isEl(x + 1, y)) acc3(x + 1, y)
            else acc3(x, y + 1))))
      }
    }
    allBestPathsAcc(0, 0, Map())
  }

  override def toString: String =
    field.map(row => row.map(("%0" + elSize.toString + "d").format(_)).mkString(" ")).mkString(Properties.lineSeparator)

  def pathToString(path: List[(Int, Int)]): String =
    field.zipWithIndex.map(row =>
      row._1.zipWithIndex.map(col =>
        if (path.contains((col._2, row._2))) ("%0" + elSize.toString + "d").format(col._1) else "." * elSize)
      .mkString(" ")).mkString(Properties.lineSeparator)
}

object NumberField {
  def apply(field: Seq[Seq[Int]]) = new NumberField(field)
  def apply(field: String) = new NumberField(fromString(field))
  def apply(width: Int, height: Int, numbers: Set[Int]): NumberField = {
    val nums = numbers.toVector
    val field = (0 until height).map(_ => (0 until width).map(_ => nums(Random.nextInt(nums.length))))
    new NumberField(field)
  }

  def isValid(field: Seq[Seq[Int]]): Boolean =
    field.nonEmpty && field.head.nonEmpty && !field.exists(row => row.length != field.head.length)

  def fromString(field: String): Seq[Seq[Int]] =
    field.split(Properties.lineSeparator).filterNot(_.trim.isEmpty).map(row => row.split("\\s+").toSeq.map(_.toInt))
}
