package wordsegment

class Segmenter(
  val unigrams: Map[String, Double],
  val bigrams: Map[String, Double],
  val words: Seq[String],
  val total: Double,
  val limit: Int
) {
  def score(word: String, previous: Option[String] = None): Double = {
    previous match {
      case None => {
        if(unigrams.contains(word)) unigrams(word) / total
        else 10.0 / (total * math.pow(10 , word.length))
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
  
  def segment(text: String): Seq[String] = {
    var memo = collection.mutable.Map.empty[(String, String), (Double, Seq[String])]
    def search(text: String, previous: String = "<s>"): (Double, Seq[String]) = {
      text match {
        case "" => (0.0, Seq.empty[String])
        case _  => {
          divide(text).map(t => {
            val (prefix: String, suffix: String) = t
            val prefix_score = math.log10(score(prefix, Some(previous)))

            val pair = (suffix, prefix)
            if(!memo.contains(pair)) memo.put(pair, search(suffix, prefix))
            val (suffix_score, suffix_words) = memo(pair)

            (prefix_score + suffix_score, Seq(prefix) ++ suffix_words)
          }).maxBy(_._1)
        }
      }
    }

    val clean_text = wordsegment.clean(text)
    val size = 250
    
    var prefix = ""
    var words = Seq.empty[String]
    0.to(clean_text.length).by(size).foreach(offset => {
      val chunk_end = if(offset + size < clean_text.length) offset + size else clean_text.length
      val chunk = clean_text.substring(offset, chunk_end)
      val (_: Double, chunk_words: Seq[String]) = search(prefix + chunk)
      prefix = chunk_words.takeRight(5).mkString
      words ++= chunk_words.dropRight(5)
    })
    val (_: Double, prefix_words: Seq[String]) = search(prefix)
    words ++ prefix_words
  }

}
