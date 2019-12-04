val passwordRange = Range(284639, 748759)
val passwordArrays = passwordRange.map(x => (x, x.toString.toArray.map(_.toString.toInt)))

val validPasswords =
  passwordArrays.map(x => (x._1, x._2 zip x._2.tail))  // create tuples (123456, [(1,2), (2,3)...])
  .filter(x => x._2.forall(y => y._1 <= y._2))  // ensure ascending values
  .filter(x => x._2.exists(y => y._1 == y._2))  // make sure there's a matching tuple

// Part 1
validPasswords.length

// Part 2
val validerPasswords =
validPasswords.map(x=> (x._1, x._1.toString.toArray.map(_.toString.toInt)))
    .map(x => (x._1, x._2.groupMapReduce(_.intValue)(_ => 1)(_ + _).values))
    .filter(x => x._2.exists(_==2))

validerPasswords.length
