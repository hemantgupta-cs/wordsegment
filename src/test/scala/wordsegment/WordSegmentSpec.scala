package wordsegment

import org.scalatest._

import wordsegment._

class WordSegmentSpec extends FlatSpec with Matchers {

  "clean" should "compact sentence to word" in {
    clean("Can't buy me love!") should be ("cantbuymelove")
  }

  "test" should "be in UNIGRAMS" in {
    default.unigrams should contain key "test"
  }
  
  "in the" should "be in BIGRAMS" in {
    default.bigrams should contain key "in the"
  }

  "choosespain" should "be `choose spain`" in {
    val result = Seq("choose", "spain")
    segment(result.mkString) should be (result)
  }
  
  "thisisatest" should "be `this is a test`" in {
    val result = Seq("this", "is", "a", "test")
    segment(result.mkString) should be (result)
  }
  
  "wheninthecourseofhumaneventsitbecomesnecessary" should "be `when in the course of human events it becomes necessary`" in {
    val result = Seq("when", "in", "the", "course", "of", "human", "events", "it", "becomes", "necessary")
    segment(result.mkString) should be (result)
  }
  
  "whorepresents" should "be `who represents`" in {
    val result = Seq("who", "represents")
    segment(result.mkString) should be (result)
  }

  "expertsexchange" should "be `experts exchange`" in {
    val result = Seq("experts", "exchange")
    segment(result.mkString) should be (result)
  }
  
  "speedofart" should "be `speed of art`" in {
    val result = Seq("speed", "of", "art")
    segment(result.mkString) should be (result)
  }
  
  "nowisthetimeforallgood" should "be `now is the time for all good`" in {
    val result = Seq("now", "is", "time", "for", "all", "good")
    segment(result.mkString) should be (result)
  }
  
  "itisatruthuniversallyacknowledged" should "be `it is a truth universally acknowledged`" in {
    val result = Seq("it", "is", "a", "truth", "universally", "acknowledged")
    segment(result.mkString) should be (result)
  }
  
  "itwasabrightcolddayinaprilandtheclockswerestrikingthirteen" should "be `it was a bright cold day in april and the clocks were striking thirteen`" in {
    val result = Seq("it", "was", "a", "bright", "cold", "day", "in", "april", "and", "the", "clocks", "were", "striking", "thirteen")
    segment(result.mkString) should be (result)
  }
  
  "itwasthebestoftimesitwastheworstoftimesitwastheageofwisdomitwastheageoffoolishness" should "be `it was the best of times it was the worst of times it was the age of wisdom it was the age of foolishness`" in {
    val result = Seq(
      "it", "was", "the", "best", "of", "times",
      "it", "was", "the", "worst", "of", "times",
      "it", "was", "the", "age", "of", "wisdom",
      "it", "was", "the", "age", "of", "foolishness"
    )
    segment(result.mkString) should be (result)
  }
  
  "asgregorsamsaawokeonemorningfromuneasydreamshefoundhimselftransformedinhisbedintoagiganticinsect" should "be `as gregor samsa awoke one morning from uneasy dreams he found himself transformed in his bed into a gigantic insect`" in {
    val result = Seq(
      "as", "gregor", "samsa", "awoke", "one", "morning", "from", "uneasy",
      "dreams", "he", "found", "himself", "transformed", "in", "his", "bed",
      "into", "a", "gigantic", "insect"
    )
    segment(result.mkString) should be (result)
  }

  "inaholeinthegroundtherelivedahobbitnotanastydirtywetholefilledwiththeendsofwormsandanoozysmellnoryetadrybaresandyholewithnothinginittositdownonortoeatitwasahobbitholeandthatmeanscomport" should "be `in a hole in the ground there lived a hobbit not a nasty dirty wet hole filled with the ends of worms and an oozy smell nor yet a dry bare sandy hole with nothing in it to sit down on or to eat it was a hobbit hole and that means comfort`" in {
    val result = Seq(
      "in", "a", "hole", "in", "the", "ground", "there", "lived", "a",
      "hobbit", "not", "a", "nasty", "dirty", "wet", "hole", "filled", "with",
      "the", "ends", "of", "worms", "and", "an", "oozy", "smell", "nor",
      "yet", "a", "dry", "bare", "sandy", "hole", "with", "nothing", "in",
      "it", "to", "sit", "down", "on", "or", "to", "eat", "it", "was", "a",
      "hobbit", "hole", "and", "that", "means", "comfort"
    )
    segment(result.mkString) should be (result)
  }

  "faroutintheunchartedbackwatersoftheunfashionableendofthewesternspiralarmofthegalaxyliesasmallunregardedyellowsun" should "be `far out in the uncharted backwaters of the unfashionable end of the western spiral arm of the galaxy lies a small un regarted yellow sun`" in {
    val result = Seq(
      "far", "out", "in", "the", "uncharted", "backwaters", "of", "the",
      "unfashionable", "end", "of", "the", "western", "spiral", "arm", "of",
      "the", "galaxy", "lies", "a", "small", "un", "regarded", "yellow", "sun"
    )
    segment(result.mkString) should be (result)
  }
}
