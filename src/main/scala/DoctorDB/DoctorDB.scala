/*

package DoctorDB

import java.io.File

import scala.collection.immutable.HashMap
import com.github.tototoshi.csv._

class DoctorDB {

}

object DoctorDB {
  def fromFile(csvFile: String): List[Map[String, String]] = {
    val reader = CSVReader.open(new File(csvFile))
    reader.allWithHeaders()
  }

  def mapByProperty(doctors: List[Map[String, String]], property: String):
    Map[String, List[Map[String, String]]] =
      (for {
          d <- doctors
          if d.isDefinedAt(property)
        } yield (d(property), d)).groupBy(_._1).mapValues(xs => xs.map(v => v._2))

  def mapByProperty(doctors: List[Map[String, String]], properties: List[String]):
    Map[String, List[Map[String, String]]] = {
    properties.map(p => mapByProperty(doctors, p)).groupBy
  }
}

*/