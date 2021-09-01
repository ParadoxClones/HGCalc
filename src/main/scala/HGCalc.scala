import scala.collection.immutable.List

class HGCalc (
             var defaultPopSize: Int = 40,
             var defaultNumSuccess: Int = 3,
             var defaultSampleSize: Int = 5,
             var defaultTargetSuccess: Int = 1,
             var defaultMax: Int = 40,
             var dependent: String = "none",
             var symbol: Char = '≥'
           ) {
  def copy(): HGCalc = new HGCalc(defaultPopSize, defaultNumSuccess, defaultSampleSize, defaultTargetSuccess, defaultMax, dependent, symbol)

  def calculate(): List[Result] = {
    // list of possible values to check
    val max = dependent match {
      case "popSize" => defaultMax
      case "numSuccess" => defaultPopSize
      case "sampleSize" => defaultPopSize
      case "targetSuccess" => defaultMax
      case _ => 1
    }

    // loops over list of possible values with the chosen dependent variable
    for (i <- List.range(0, max + 1)) yield {
      // sets the value to i if it is the dependent variable
      val popSize =
        if (dependent == "popSize") i
        else defaultPopSize
      val numSuccess =
        if (dependent == "numSuccess") i
        else defaultNumSuccess
      val sampleSize =
        if (dependent == "sampleSize") i
        else defaultSampleSize
      val targetSuccess =
        if (dependent == "targetSuccess") i
        else defaultTargetSuccess

      // handles the different symbols
      val chance = symbol match {
        case '>' => getListHelper('≥', popSize, numSuccess, sampleSize, targetSuccess + 1)
        case '≤' => 1 - getListHelper('≥', popSize, numSuccess, sampleSize, targetSuccess + 1)
        case '<' => 1 - getListHelper('≥', popSize, numSuccess, sampleSize, targetSuccess)
        case _ => getListHelper(symbol, popSize, numSuccess, sampleSize, targetSuccess)
      }

      Result(i, chance)
    }
  }

  // solves the hypergeometric calculation
  private def getListHelper(s: Char, ps: Int, ns: Int, ss: Int, ts: Int, chance: Double = 1): Double = {
    val res = if (ss < ts) 0
      else if (s == '≥' && (ts == 0 || ps < ns)) chance
      else if (s == '=' && (ss == 0 || ps == 0)) if (ts == 0 && ps != 0) chance else 0
      else (getListHelper(s, ps - 1, ns - 1, ss - 1, ts - 1, chance * ns/ps)
        + getListHelper(s, ps - 1, ns, ss - 1, ts, chance * (1 - ns * 1.0/ps)))
    res
  }
}