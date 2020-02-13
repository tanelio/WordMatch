import java.io.File
import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}
import scala.io.Source
import scala.sys.process._
import scala.collection.immutable.HashSet
import scala.collection.mutable.HashMap
import scala.util.control.Breaks._

object WordMatch extends App {
  val words: /*mutable.*/ HashMap[String, (HashSet[Char], String)] = /*mutable.*/ HashMap()
  var discq = 0
  for (dict <- List("adj", "adv", "noun", "verb")) Initialize(dict)
  println(s"Disqualified = $discq, words = ${words.size}")
  val w = "liberation"
  println(s"test: $w")
  findWord(w)

  def findWord(w: String) {
    val word = HashSet() ++ w.toArray.toSet // Get the hashSet of the word
    var c = 0
    for ((wrd, (hsh, expl)) <- words) {
      c += 1
//      println(s"$c. $wrd ${hsh} (${clean(wrd)})")
      try {
        if (hsh.subsetOf(word))
          println(s"Subset: ${wrd}: ${expl}")
      } catch {
        case e: Exception =>
          println(s"Exception - $e")
          println(s"$c. $wrd ${hsh} (${clean(wrd)})")
      }
    }
  }

  def clean(w: String): String = {
    var word = w.toLowerCase
    if (word.contains('(') || word.contains('-') || word.contains('_'))  {
      while (word.contains('(')) word = word.init
      if (word.contains('-'))    word = word.replaceAllLiterally("-", "")
      if (word.contains('_'))    word = word.replaceAllLiterally("_", "")
    }
    word
  }

  def Initialize(what: String): Unit = {
    println(s"reading $what")
    val adj = new File(s"dict/data.$what")
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    val bufferedSource = Source.fromFile(adj)
    val lineI = bufferedSource.getLines.filter(p => p.head.isDigit)
    do { // Skip pre-amble
      val l = lineI.next.split('|')
      val c = l(0).split(' ').drop(4)
      breakable {
        for (wd <- c) {
          if (!wd.head.isLetter || wd.length == 1) break
          if (wd.contains('.')) {
            println(s"Disqualifying: $wd")
            discq += 1
          } else {
            words.addOne(wd, (HashSet() ++ clean(wd).toArray.toSet, l(1).tail))
            if (words.size < 40) println(s"word=$wd, expl=${l(1).tail}")
          }
        }
      }
    } while (lineI.hasNext)
    bufferedSource.close
  }
}

