import util.Properties

/**
  * Mars Lander has a two-dimensional map representing Mars surface. "!" character represents a point of interest (POI).
  * A map may have obstacles represented by "?". The lander travels from its landing point to each of the POIs and back.
  */
class MarsLander(map: List[List[Char]], landing: (Int, Int)) {
  require(map.nonEmpty, "Map must have at least one row")
  require(map.head.nonEmpty, "Map must have at least one column")
  require(map.forall(r => map.forall(r2 => r.length == r2.length)), "Map must be rectangular")
  require(landing._1 < map.size, "Landing X coordinate must be on the map")
  require(landing._2 < map.head.size, "Landing Y coordinate must be on the map")

  val pois: List[(Int, Int)] =
    for {
      row <- map.zipWithIndex
      cell <- row._1.zipWithIndex
      if cell._1 == MarsLander.POI
    } yield (row._2, cell._2)

  /**
    * Obstacle-less impl
    */
  def pathLengthToPoi(poi: (Int, Int)): Int = (Math.abs(landing._1 - poi._1) + Math.abs(landing._2 - poi._2)) * 2
  def pathLengthToAllPois: Int = pois.map(pathLengthToPoi).sum
}

object MarsLander {
  val POI = '!'
  val OBSTACLE = '?'

  def apply(map: String, landing: (Int, Int)) =
    new MarsLander(map.trim.split(Properties.lineSeparator).map(_.toList).toList, landing)
}
