package streams

object ResolveFunc extends App {
  def resolveFunc: PartialFunction[(Double, Double), Double] = {
    case (x, k) if x != k =>
      if (x > 1 && x < k)
        k
      else
        fact(x)
  }

  def fact(x: Double): Double = {
    if (x <= 0)
      1
    else
      x * fact(x - 1)
  }



  def toList(range: Seq[Int], k: Double): List[Double] =
    range.map(x => (x.toDouble, k)).collect(resolveFunc).toList


  val list = toList(-250 to 250, 2)

  println(list)
  println(list.filter(num => num < 1000))
  println(list.foldRight(0.0) {
    case (acc, num) => acc + num
  })
  println(list.indexWhere(num => num == 0))
  println(list.exists(num => num % 3 == 0))
  print(list.find(num => num < 135))


}
