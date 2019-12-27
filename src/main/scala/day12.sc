import scala.collection.mutable
// First test values
//val moon1 = ((-1, 0, 2), (0, 0, 0))
//val moon2 = ((2, -10, -7), (0, 0, 0))
//val moon3 = ((4, -8, 8), (0, 0, 0))
//val moon4 = ((3, 5, -1), (0, 0, 0))

// Actual puzzle input
val moon1 = ((16, -11, 2), (0, 0, 0))
val moon2 = ((0, -4, 7), (0, 0, 0))
val moon3 = ((6, 4, -10), (0, 0, 0))
val moon4 = ((-3, -2, -4), (0, 0, 0))

var moons = List(moon1, moon2, moon3, moon4)

def compare(int1: Int, int2: Int): Int = {
  if (int1 > int2) -1
  else if (int1 < int2) 1
  else 0
}

def calculateGravity(moon: (Int, Int, Int),
                     other: (Int, Int, Int)): (Int, Int, Int) = {
  (compare(moon._1, other._1),
  compare(moon._2, other._2),
  compare(moon._3, other._3))
}

def addTuples(one: (Int, Int, Int),
              two: (Int, Int, Int)): (Int, Int, Int) = {
  (one._1 + two._1, one._2 + two._2, one._3 + two._3)
}

def calculateEnergy(moon: ((Int, Int, Int), (Int, Int, Int))): Int = {
  val pot = List(moon._1._1, moon._1._2, moon._1._3).map(math.abs).sum
  val kin = List(moon._2._1, moon._2._2, moon._2._3).map(math.abs).sum
  pot*kin
}

def processStep(moonsList: List[((Int, Int, Int), (Int, Int, Int))]) = {
  moonsList.map(x => (x._1, List(x._2, moonsList.map(y => if (x != y) calculateGravity(x._1, y._1) else (0, 0, 0)).reduceLeft((x, y) => addTuples(x, y))).reduceLeft((x, y) => addTuples(x, y)))).map(x => (addTuples(x._1, x._2), x._2))
}

def checkZeroSpeed(moonsList: List[((Int, Int, Int), (Int, Int, Int))]): List[Boolean] = {
  val moonSpeeds = moonsList.flatMap(x => List(x._2))
  val xSpeeds = moonSpeeds.flatMap(x => List(x._1)).forall(_ == 0)
  val ySpeeds = moonSpeeds.flatMap(x => List(x._2)).forall(_ == 0)
  val zSpeeds = moonSpeeds.flatMap(x => List(x._3)).forall(_ == 0)
  List(xSpeeds, ySpeeds, zSpeeds)
}

// part 1
for(c <- 1 to 1000) {
  moons = processStep(moons)
}
moons
println(moons.map(calculateEnergy).sum)

// part 2
// fixme this is ugly and requires manual calculation and doubling of the lcm
moons = List(moon1, moon2, moon3, moon4)
var pastStates = Set(moons)
moons = processStep(moons)
while(!pastStates.contains(moons)){
  pastStates = pastStates + moons
  moons = processStep(moons)
  val zeroes = checkZeroSpeed(moons)
  if (zeroes.contains(true)) {
    println(zeroes, pastStates.size)
  }
  if (pastStates.size >= 1000000) {pastStates = pastStates + moons}
}

println(pastStates.size)