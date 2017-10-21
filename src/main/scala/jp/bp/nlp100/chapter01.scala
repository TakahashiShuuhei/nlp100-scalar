package jp.bp.nlp100

import scala.util.Random

object Chapter01 {
  def ngram[A](n: Int)(xs: Iterable[A]): Iterator[Iterable[A]] = {
    xs.sliding(n)
  }

  def template[A, B, C](x: A, y: B, z: C): String = {
    s"${x}時の${y}は${z}"
  }

  def cipher(s: String): String = {
    s.map {
      case c: Char if c.isLower => (219 - c.toInt).toChar
      case c: Char => c
    }.mkString
  }

  def typoglycemia(s: String): String = {
    val transform = (s:String) => {
      val len = s.length
      val body = s.substring(1, len - 1)
      s.head + Random.shuffle(body.toList).mkString + s.last
    }

    s.split(" ")
      .map{
        case s: String if s.length > 4 => transform(s)
        case s: String => s
      }.mkString(" ")
  }
}