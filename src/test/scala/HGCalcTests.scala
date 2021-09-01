import org.scalatest.FunSuite

class HGCalcTests extends FunSuite {
  test("succeeding in population of size 10 with 1 success") {
    val res = new HGCalc(10, 1, 1, 1)
    assert(res.calculate().head.chance == 0.1)
  }

  test("succeeding in population of size 40 with 3 successes") {
    val res = new HGCalc(40, 3, 1, 1)
    assert(res.calculate().head.chance == 0.075)
  }

  test("succeeding in population of size 40 with 3 successes and sample size 5") {
    val res = new HGCalc(40, 3, 5, 1)
    assert(res.calculate().head.chance == 0.33755060728744946)
  }

  test("getting 2 successes in population of size 40 with 3 successes and sample size 5") {
    val res = new HGCalc(40, 3, 5, 2)
    assert(res.calculate().head.chance == 0.03643724696356276)
  }
}
