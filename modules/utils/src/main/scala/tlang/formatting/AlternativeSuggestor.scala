package tlang
package formatting

case class AlternativeSuggestor() {

  private val MinLength      = 3
  private val MaxSuggestions = 5

  def apply(name: String, alternatives: List[String]): Suggestion = {
    if (name.length < MinLength)
      return Suggestion(Nil)

    Suggestion(
      alternatives
        .filter { _.length >= MinLength }
        .map { target => DistanceData(name, target) }
        .filter { _.isAcceptable }
        .sortBy { -_.similarity }
        .map { _.target }
        .take { MaxSuggestions }
    )
  }

  private case class DistanceData(source: String, target: String) {

    val AcceptableSimilarity = 0.5

    val distance    : Int     = Distance(source, target)
    val similarity  : Double  = 1.0 - (distance.toDouble / math.max(source.length, target.length))
    val isAcceptable: Boolean = similarity > AcceptableSimilarity

  }
}

object Distance {

  def apply(source: String, target: String): Int = {
    if (source.length == 0) return target.length
    if (target.length == 0) return source.length
    if (target == source) return 0
    if (source.toLowerCase == target.toLowerCase) return 1

    distance(source, target)
  }

  // Damerau–Levenshtein distance
  //
  // Levenshtein distance is the number of deletions, insertions and replacements it takes
  // to turn source into target. The difference in Dameraus version is that a transposition
  // e.g the distance between ABC to ACB is one instead of two. These operations account for
  // around 80 % of misspellings.
  private def distance(source: String, target: String): Int = {
    val S = source.length
    val T = target.length

    val dist = Array.ofDim[Int](S + 1, T + 1)
    for (i <- 0 to S) dist(i)(0) = i
    for (j <- 0 to T) dist(0)(j) = j

    for (i <- 1 to S; j <- 1 to T) {
      val cost = if (source(i - 1) == target(j - 1)) 0 else 1

      dist(i)(j) = min(
        dist(i - 1)(j) + 1, // Deletion
        dist(i)(j - 1) + 1, // Insertion
        dist(i - 1)(j - 1) + cost // Substitution
      )

      if (i > 1 && j > 1 && source(i - 1) == target(j - 2) && source(i - 2) == target(j - 1))
        dist(i)(j) = min(dist(i)(j), dist(i - 2)(j - 2) + cost) // Transposition
    }

    dist(S)(T)
  }

  private def min(values: Int*) = values.min

}

// This wrapper is used so we can pick it up in the Error String Context and format it properly
case class Suggestion(suggestions: List[String])
