import util.Properties

/**
  * Class representing a two-dimensional "field" of integers.
  * Each element of the inner array is a row in the field.
  * @param field represents two-dimension field of numbers.
  */
class NumberField (field: Array[Array[Int]]) {
  require(NumberField.isValid(field), "Invalid number field specified")
  val height: Int = field.length
  val width: Int = field(0).length
  val elSize: Int = field.map(row => row.max).max.toString.length

  def el(x: Int, y: Int): Int = {
    require(y >= 0 && y < height && x >= 0 && x < width, s"Invalid co-ordinates ($x, $y) specified")
    field(y)(x)
  }

  def evalPath(path: List[(Int, Int)]): Int = path.foldLeft(0)((e, cell) => e + el(cell._1, cell._2))

  def bestPath: List[(Int, Int)] = {
    def bestPathAcc(x: Int, y: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      (x, y) match {
        case (right, bottom) if right == width - 1 && bottom == height - 1 => (x, y) :: acc
        case (right, other) if right == width - 1 => bestPathAcc(x, y + 1, (x, y) :: acc)
        case (other, bottom) if bottom == height - 1 => bestPathAcc(x + 1, y, (x, y) :: acc)
        case _ =>
          val overPath = bestPathAcc(x + 1, y, (x, y) :: acc)
          val downPath = bestPathAcc(x, y + 1, (x, y) :: acc)
          if (evalPath(overPath) > evalPath(downPath)) overPath else downPath
      }
    }
    bestPathAcc(0, 0, List()).reverse
  }

  override def toString: String =
    field.map(row => row.map(("%0" + elSize.toString + "d").format(_)).mkString(" ")).mkString(Properties.lineSeparator)
}

object NumberField {
  def apply(field: Array[Array[Int]]) = new NumberField(field)
  def apply(field: String) = new NumberField(fromString(field))

  def isValid(field: Array[Array[Int]]): Boolean =
    field.nonEmpty && field(0).nonEmpty && !field.exists(row => row.length != field(0).length)

  def fromString(field: String): Array[Array[Int]] =
    field.split(Properties.lineSeparator).filterNot(_.trim.isEmpty).map(row => row.split("\\s+").map(_.toInt))
}
