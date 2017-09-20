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

  def el(x: Int, y: Int): Int = {
    require(y >= 0 && y < height && x >= 0 && x < width, s"Invalid co-ordinates ($x, $y) specified")
    field(y)(x)
  }

  def evalPath(path: List[(Int, Int)]): Int = path.foldLeft(0)((e, cell) => e + el(cell._1, cell._2))

  def bestPath: List[(Int, Int)] = {
    def bestPathAcc(x: Int, y: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      val accNext = (x, y) :: acc
      (x, y) match {
        case (right, bottom) if right == width - 1 && bottom == height - 1 => accNext
        case (right, other) if right == width - 1 => bestPathAcc(x, y + 1, accNext)
        case (other, bottom) if bottom == height - 1 => bestPathAcc(x + 1, y, accNext)
        case _ =>
          val overPath = bestPathAcc(x + 1, y, accNext)
          val downPath = bestPathAcc(x, y + 1, accNext)
          if (evalPath(overPath) > evalPath(downPath)) overPath else downPath
      }
    }
    bestPathAcc(0, 0, List()).reverse
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
  def apply(height: Int, width: Int, numbers: Set[Int]) =
    new NumberField(Seq.fill[Seq[Int]](height)(Seq.fill[Int](width)(numbers.take(Random.nextInt(numbers.size)).head)))

  def isValid(field: Seq[Seq[Int]]): Boolean =
    field.nonEmpty && field.head.nonEmpty && !field.exists(row => row.length != field.head.length)

  def fromString(field: String): Seq[Seq[Int]] =
    field.split(Properties.lineSeparator).filterNot(_.trim.isEmpty).map(row => row.split("\\s+").toSeq.map(_.toInt))
}
