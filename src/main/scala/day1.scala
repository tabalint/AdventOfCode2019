import scala.annotation.tailrec
import scala.io.Source

object day1 extends App {

  // get the raw input and convert to floats
  val raw_input = Source.fromResource("day1_input.txt")
  val lines = try raw_input.getLines.toList.map(_.toDouble) finally raw_input.close()

  // process initial fuel needs
  val fuel = lines.map(x => math.floor(x/3.0) - 2)

  // end of part 1
  println(fuel.sum)

  // part 2, we have to recursively treat the fuel that we calculated before
  @tailrec
  def calcFuel(a: List[Double]): List[Double] = {
        a match {
          case _ if a.head > 8 => calcFuel((math.floor(a.head / 3.0) - 2) +: a)
          case _ => a
        }
  }

  println(fuel.map(x => calcFuel(List(x)).sum).sum)
}
