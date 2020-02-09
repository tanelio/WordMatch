import java.io.File
import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}
import scala.io.Source
import scala.sys.process._
import scala.collection.immutable.HashSet
import scala.collection.mutable
import scala.util.control.Breaks._

object WordMatch extends App {
  var words: mutable.HashMap[String, (HashSet[Char], String)] = mutable.HashMap()
  var d = 0
  Initialize("adj")
  Initialize("adv")
  Initialize("noun")
  Initialize("verb")
  println(s"Disqualified = $d, words = ${words.size}")

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
        for (wd <- c /*if wd.head.isLetter*/) {
          if (!wd.head.isLetter || wd == "n" || wd == "a") break
          if (wd.contains('.')) {
            println(s"Disqualifying: $wd")
            d += 1
          } else {
            if (wd.contains('(') || wd.contains('-') || wd.contains('_'))  {
              var w = wd
              while (w.contains('(')) w = w.init
              if (w.contains('-'))    w.replaceAllLiterally("-", "")
              if (w.contains('_'))    w.replaceAllLiterally("_", "")
              words.addOne(wd, (HashSet() ++ w.toArray.toSet, (l(1).tail)))
            } else
              words.addOne(wd, (HashSet() ++ wd.toArray.toSet, (l(1).tail)))
            if (words.size < 40) println(s"word=$wd, expl=${l(1).tail}")
          }
        }
      }
    } while (lineI.hasNext)
    bufferedSource.close
  }
}

