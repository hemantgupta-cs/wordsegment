package wordsegment

object Searcher {
  val default = new Searcher(
    wordsegment.EMBEDDED_UNIGRAMS,
    wordsegment.EMBEDDED_BIGRAMS,
    wordsegment.EMBEDDED_WORDS,
    wordsegment.EMBEDDED_TOTAL,
    wordsegment.EMBEDDED_LIMIT
  )
}

class Searcher(
  unigrams: Map[String, Double],
  bigrams: Map[String, Double],
  words: Seq[String],
  total: Double,
  limit: Int
) {

  def score(word: String, previous: Option[String] = None): Double = {
    previous match {
      case None => {
        if(unigrams.contains(word)) 
          unigrams(word) / total
        else
          10.0 / (total * math.pow(10, word.length))
      }
      case _ => {
        val bigram = s"$previous $word"
        if(bigrams.contains(bigram) && unigrams.contains(previous.get))
          bigrams(bigram) / total / score(previous.get)
        else
          score(word)
      }
    }
  }

  def divide(text: String): Seq[(String, String)] = {
    1.to(math.min(text.length, limit))
      .map(n => (text.substring(0, n), text.substring(n)))
  }

  val memo = collection.mutable.Map.empty[(String, String), (Double, Seq[String])]
  def apply(text: String, previous: String = "<s>"): (Double, Seq[String]) = {
    text match {
      case "" => (0.0, Seq.empty[String])
      case _  => {
        import Ordering.Implicits._
        val candidates = divide(text).map(p => p match {
          case (prefix: String, suffix: String) => {
            val prefix_score = math.log10(score(prefix, Some(previous)))
            val pair = (suffix, prefix)

            if(!memo.contains(pair)) memo.put(pair, apply(suffix, prefix))
            val (suffix_score, suffix_words) = memo(pair)

            (prefix_score + suffix_score, Seq(prefix) ++ suffix_words)
          }
        })
        //candidates.sorted.foreach(println)
        candidates.max
      }
    }
  }
}
