package wordsegment

import org.scalatest._

import wordsegment._

class SearcherSpec extends FlatSpec with Matchers {

  "divide" should "make pairs of parts of string" in {
    val search = Searcher.default
    search.divide("asdf") should be (Seq(
      ("a", "sdf"), ("as", "df"), ("asd", "f"), ("asdf", "")
    ))
  }

  "ScoreOps" should "compare two objects" in {
    import Ordering.Implicits._
    (3.0, Seq("a", "b", "c")) should be < (3.0, Seq("a", "b", "c", "d"))
    (3.0, Seq("a", "b", "c")) should be < (3.0, Seq("a", "b", "d"))
    (5.0, Seq("a", "b", "c")) should be > (3.0, Seq("a", "b", "c", "d"))
    (5.0, Seq("a", "b", "d")) should be > (3.0, Seq("a", "b", "c", "d"))
    (3.0, Seq("a", "b", "d")) should be > (3.0, Seq("a", "b", "c", "d"))
  }
  
}
