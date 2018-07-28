package wordsegment

import java.io._
import scala.io.Source

package object wordsegment {
  val ALPHABET = "abcdefghijklmnopqrstuvwxyz0123456789"
  val EMBEDDED_UNIGRAMS_PATH = "wordsegment/unigrams.txt"
  val EMBEDDED_BIGRAMS_PATH = "wordsegment/bigrams.txt"
  val EMBEDDED_WORDS_PATH = "wordsegment/words.txt"
  val EMBEDDED_TOTAL = 1024908267229.0
  val EMBEDDED_LIMIT = 24
  
  def clean(text: String): String = {
    text.toLowerCase
      .filter(l => ALPHABET.contains(l))
      .mkString
  }

  def parsePairs(stream: InputStream): Map[String, Double] = {
    require(stream != null)
    val lines = Source.fromInputStream(stream).getLines
    val pairs = lines.map(l => l.split("\t"))
    pairs.map(pair => pair match {
      case Array(word: String, score: String) => (word, score.toDouble)
    }).toMap
  }
  
  def parsePairsFromResource(path: String): Map[String, Double] = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    try {
      parsePairs(stream)
    } finally {
      stream.close
    }
  }

  def loadWords(stream: InputStream): Seq[String] = {
    Source.fromInputStream(stream).getLines.toSeq
  }

  def parseWordsFromFile(path: String): Seq[String] = {
    val stream = new FileInputStream(new File(path))
    try {
      loadWords(stream)
    } finally {
      stream.close
    }
  }
  
  def loadWordsFromResource(path: String): Seq[String] = {
    val stream = getClass.getClassLoader.getResourceAsStream(path)
    try {
      loadWords(stream)
    } finally {
      stream.close
    }
  }

  def loadWordsFromFile(path: String): Seq[String] = {
    val stream = new FileInputStream(new File(path))
    try {
      loadWords(stream)
    } finally {
      stream.close
    }
  }

  def default: Segmenter = {
    new Segmenter(
      parsePairsFromResource(EMBEDDED_UNIGRAMS_PATH),
      parsePairsFromResource(EMBEDDED_BIGRAMS_PATH),
      loadWordsFromResource(EMBEDDED_WORDS_PATH),
      EMBEDDED_TOTAL,
      EMBEDDED_LIMIT
    )
  }

  def segment(text: String): Seq[String] = default.segment(text)
  
}
