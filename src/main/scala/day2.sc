val raw_input = List(1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,1,9,19,1,19,5,23,2,23,13,27,1,10,27,31,2,31,6,35,1,5,35,39,1,39,10,43,2,9,43,47,1,47,5,51,2,51,9,55,1,13,55,59,1,13,59,63,1,6,63,67,2,13,67,71,1,10,71,75,2,13,75,79,1,5,79,83,2,83,9,87,2,87,13,91,1,91,5,95,2,9,95,99,1,99,5,103,1,2,103,107,1,10,107,0,99,2,14,0,0)
val input_zip = raw_input.indices zip raw_input

// function to run the opcode calculator
// takes the whole mutable map, runs the calculation until it hits a 99, then returns the new map
def opcode_calculator(this_map: collection.mutable.Map[Int, Int]): collection.mutable.Map[Int, Int] = {
  var ix = 0

  while(true){
  val op = this_map(ix)
  op match {
    case 1 => this_map(this_map(ix+3)) = this_map(this_map(ix+1)) + this_map(this_map(ix+2))
    ix += 4
    case 2 => this_map(this_map(ix+3)) = this_map(this_map(ix+1)) * this_map(this_map(ix+2))
    ix += 4
    case 99 => return this_map
    }
  }
  null
}

// Part 1
def part1(): Int = {
  val this_map = collection.mutable.Map(input_zip.toMap.toSeq: _*)
  this_map(1) = 12
  this_map(2) = 2

  opcode_calculator(this_map)(0)
}

// Part 2
def part2(): Int = {
  for (x <- 0 to 100) {
    for (y <- 0 to 100) {
      val this_map = collection.mutable.Map(input_zip.toMap.toSeq: _*)
      this_map(1) = x
      this_map(2) = y

      val calculated = opcode_calculator(this_map)(0)
      if (calculated == 19690720) {
        return 100 * x + y
      }
    }
  }
  0
}

part1()
part2()
