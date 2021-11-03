package streams

import streams.ResolveFunc.{resolveFunc, list}

class ResolveFunctionSuite extends munit.FunSuite {
  val testFunctionSolver: ResolveFunc.type = ResolveFunc
  val list: List[Double] = testFunctionSolver.toList(-250 to 250, 2)
  test("list.length ") {
    assertEquals(list.length, 500)
  }

  test("list.filter ") {
    for (elem <- list.filter(num => num < 400))
      assert(elem < 400)
  }

  test("list.foldRight ") {
    val summary = list.foldRight(0.0) {
      case (acc, num) => acc + num
    }
    var acc: Double = 0
    for (i <- -250 to 250) {
      try {
        acc += resolveFunc(i, 2)
      }
      catch {
        case e: scala.MatchError => acc += 0
      }
    }
    assertEquals(summary, acc)
  }

  test("list.exists ") {
    assert(list.exists(num => num % 3 == 0))
  }

  test("list.find ") {
    assert(list.find(num => num < 135).get < 135)
  }
}
