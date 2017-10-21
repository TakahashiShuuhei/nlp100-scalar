package jp.bp.nlp100

import scala.collection.JavaConverters._

import com.atilika.kuromoji.ipadic.{Token, Tokenizer}

import scala.io.Source

/**
  * Created by shuhei on 17/10/21.
  */

case class Morph(surface: String, pos: String, pos1: String, base: String)

object Chapter04 {
  val url = "http://www.cl.ecei.tohoku.ac.jp/nlp100/data/neko.txt"

  def string(): String = {
    Source.fromURL(url).mkString
  }

  def tokenize(str: String): List[Morph] = {
    val tokenizer: Tokenizer = new Tokenizer

    val tokens: List[Token] = tokenizer.tokenize(str).asScala.toList
    tokens.map((t) => {
      Morph(t.getSurface, t.getPartOfSpeechLevel1, t.getPartOfSpeechLevel2, t.getBaseForm)
    })
  }
}