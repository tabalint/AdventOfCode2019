import scala.annotation.tailrec
import scala.io.Source

val raw_input = Source.fromResource("day6_input.txt").getLines
// create map of orbiter -> orbitee
val planet_map = raw_input.map(_.split(')')).map(x => x(1) -> x(0)).toMap

// get each planet's chain within the map by recursing through the map until the key has no value
@tailrec
def getPathToRoot(map: scala.collection.immutable.Map[String, String],
                  key: String,
                  list: List[String]):
  List[String] = {
    map.getOrElse(key, Nil) match {
      case Nil => list
      case x: String => getPathToRoot(map, x, x +: list)
    }
  }

// calculate each planet's path to COM (x -> path(x))
val planet_paths = planet_map.keys.map(x => (x, getPathToRoot(planet_map, x, List.empty[String])))

// Part 1 - just sum length of every planet's list
planet_paths.foldLeft(0)((x, y) => x + y._2.length)

//Part 2 - get parts of YOU list and SAN list that aren't in common, sum sizes
val youList = planet_paths.filter(_._1 == "YOU").flatMap(_._2).toList
val santaList = planet_paths.filter(_._1 == "SAN").flatMap(_._2).toList
youList.filterNot(santaList.contains(_)).size + santaList.filterNot(youList.contains(_)).size
