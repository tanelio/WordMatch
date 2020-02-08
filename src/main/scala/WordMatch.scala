import java.io.File
import java.nio.charset.CodingErrorAction

import scala.io.{Codec, Source}
import scala.io.Source
import scala.sys.process._
import scala.collection.immutable.HashSet
import scala.collection.mutable

/*
    HashMap[HashSet, Word, Exp]
 */


object WordMatch extends App {
  Initialize

  def Initialize(): Unit = {
    val adj = new File("dict/data.adj")
    implicit val codec = Codec("UTF-8")
    codec.onMalformedInput(CodingErrorAction.REPLACE)
    codec.onUnmappableCharacter(CodingErrorAction.REPLACE)
    val bufferedSource = Source.fromFile(adj)
    val lineI = bufferedSource.getLines.filter(p => p.head.isDigit)
    var words: mutable.HashMap[HashSet[Char], (String, String)] = mutable.HashMap.empty
    var i = 1
    var d = 0
    do { // Skip pre-amble
      val l = lineI.next.split('|')
      val w = l(0).split(' ')
      if (w(4).contains('-') || w(4).contains(' ') || w(4).contains('_') || w(4).contains('.') || w(4).contains('(')) {
        println(s"Disqualifying: ${w(0)} ${w(4)}")
        d += 1
        val hs = HashSet() ++ w(4).toArray.toSet
        words.addOne(hs, (w(4), l(1).tail))
      } else {

        if (i < 10)
          println(s"$i : id=${w(0)}, word=${w(4)}, expl=${l(1).tail}")
        else
          print(s"$i\r")
      }
      i += 1
    } while (lineI.hasNext)
    println(s"Disqualified = $d, words = $i")

    bufferedSource.close


  }
}

