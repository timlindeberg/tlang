package tlang.compiler.error

object Suggestion {
  def apply(suggestion: Option[String]) = new Suggestion(suggestion)

  def unapply(arg: Suggestion): Option[String] = arg.suggestion match {
    case x: Some[String] => x
    case _               => None
  }
}

class Suggestion(val suggestion: Option[String]) {
  override def toString: String = suggestion match {
    case Some(v) => s" Did you mean $v?"
    case None    => ""
  }
}

case class NameSuggestor() {

  private val AcceptableDistance = 3
  private val MinLength          = 3


  def apply(name: String, alternatives: List[String]): Suggestion = {
    if (name.length < MinLength)
      return Suggestion(None)

    val alts = alternatives
      .filter(_.length >= MinLength)
      .map(alt => Distance(name, alt))
      .filter(_.isAcceptable)
      .sortBy(_.distance)

    val res = if (alts.isEmpty) None else Some(alts.head.alternative)
    Suggestion(res)
  }


  private case class Distance(name: String, alternative: String) {

    // Levensthein distance
    val distance: Int = {
      val dist = Array.tabulate(alternative.length + 1, name.length + 1) { (j, i) =>
        if (j == 0) i
        else if (i == 0) j
        else 0
      }

      for (j <- 1 to alternative.length; i <- 1 to name.length)
        dist(j)(i) =
          if (alternative(j - 1) == name(i - 1))
            dist(j - 1)(i - 1)
          else
            min(dist(j - 1)(i) + 1, dist(j)(i - 1) + 1, dist(j - 1)(i - 1) + 1)

      dist(alternative.length)(name.length)
    }

    val isAcceptable: Boolean = distance <= AcceptableDistance && distance < name.length

    private def min(values: Int*) = values.min
  }

}
