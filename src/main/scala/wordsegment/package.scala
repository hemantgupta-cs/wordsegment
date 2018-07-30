package wordsegment

import java.io._
import scala.io.Source

package object wordsegment {
  val ALPHABET = "abcdefghijklmnopqrstuvwxyz"
  val EMBEDDED_UNIGRAMS_RESOURCE_PATH = "wordsegment/unigrams.txt"
  val EMBEDDED_BIGRAMS_RESOURCE_PATH = "wordsegment/bigrams.txt"
  val EMBEDDED_WORDS_RESOURCE_PATH = "wordsegment/words.txt"
  val EMBEDDED_TOTAL = 1024908267229.0
  val EMBEDDED_LIMIT = 24
      
  val EMBEDDED_UNIGRAMS = parsePairsFromResource(EMBEDDED_UNIGRAMS_RESOURCE_PATH)
  val EMBEDDED_BIGRAMS = parsePairsFromResource(EMBEDDED_BIGRAMS_RESOURCE_PATH)
  val EMBEDDED_WORDS = loadWordsFromResource(EMBEDDED_WORDS_RESOURCE_PATH)
  
  def clean(text: String): String = {
    text.toLowerCase
      .filter(l => ALPHABET.contains(l))
      .mkString
  }

  def parsePairs(stream: InputStream): Map[String, Double] = {
    require(stream != null)
    val source = Source.fromInputStream(stream)
    try {
      val pairs = source.getLines.map(l => l.split("\t"))
      pairs.map(pair => pair match {
        case Array(word: String, score: String) => (word, score.toDouble)
      }).toMap
    } finally {
      source.close
    }
  }
  
  def parsePairsFromResource(path: String): Map[String, Double] = {
    parsePairs(getClass.getClassLoader.getResourceAsStream(path))
  }
  
  def parsePairsFromFile(path: String): Map[String, Double] = {
    parsePairs(new FileInputStream(new File(path)))
  }

  def loadWords(stream: InputStream): Seq[String] = {
    val source = Source.fromInputStream(stream)
    try {
      source.getLines.toSeq
    } finally {
      source.close
    }
  }

  def parseWordsFromFile(path: String): Seq[String] = {
    loadWords(new FileInputStream(new File(path)))
  }
  
  def loadWordsFromResource(path: String): Seq[String] = {
    loadWords(getClass.getClassLoader.getResourceAsStream(path))
  }

  def loadWordsFromFile(path: String): Seq[String] = {
    val stream = new FileInputStream(new File(path))
    loadWords(stream)
  }

  def segment(text: String): Seq[String] = {
    val search = Searcher.default
    
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
