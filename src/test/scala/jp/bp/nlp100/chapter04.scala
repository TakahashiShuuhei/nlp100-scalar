package jp.bp.nlp100

import org.scalatest.{BeforeAndAfter, FunSuite, Matchers}

import scala.collection.mutable

/**
  * Created by shuhei on 17/10/21.
  */

/**
  * 夏目漱石の小説『吾輩は猫である』の文章（neko.txt）をMeCabを使って形態素解析し，
  * その結果をneko.txt.mecabというファイルに保存せよ．このファイルを用いて，以下の問に対応するプログラムを実装せよ．
  *
  * なお，問題37, 38, 39はmatplotlibもしくはGnuplotを用いるとよい．
  */
class Chapter04Suite extends FunSuite with Matchers with BeforeAndAfter {
  var morphs: List[Morph] = _

  before {
    morphs = Chapter04.tokenize(Chapter04.string)
  }

  /**
    * 形態素解析結果（neko.txt.mecab）を読み込むプログラムを実装せよ．
    * ただし，各形態素は表層形（surface），基本形（base），品詞（pos），
    * 品詞細分類1（pos1）をキーとするマッピング型に格納し，
    * 1文を形態素（マッピング型）のリストとして表現せよ．
    * 第4章の残りの問題では，ここで作ったプログラムを活用せよ．
    */
  test("30. 形態素解析結果の読み込み") {
    // mecabをscalaから使うのがしんどいのでkuromojiを利用する
    morphs.foreach(println)
  }

  /**
    * 動詞の表層形をすべて抽出せよ． (31)
    * 動詞の原形をすべて抽出せよ． (32)
    */
  test("31. 動詞, 32. 動詞の原形") {
    morphs.filter(m => m.pos == "動詞")
      .foreach(println)
  }

  /**
    * サ変接続の名詞をすべて抽出せよ．
    */
  test("33. サ変名詞") {
    morphs.filter(m => m.pos == "名詞" && m.pos1 == "サ変接続")
      .foreach(println)
  }

  /**
    * 2つの名詞が「の」で連結されている名詞句を抽出せよ．
    */
  test("34. 「AのB」") {
    morphs.sliding(3)
      .filter((s) => s(0).pos == "名詞"
        && s(1).surface == "の"
        && s(2).pos == "名詞")
      .foreach(println)
  }

  /**
    * 名詞の連接（連続して出現する名詞）を最長一致で抽出せよ．
    */
  test("35. 名詞の連接") {
    // TODO すごく手続き型...
    var current: mutable.MutableList[Morph] = new mutable.MutableList()
    var longest: mutable.MutableList[Morph] = new mutable.MutableList()

    morphs.foreach((m) => {
      if (m.pos == "名詞") {
        current += m
        if (current.length > longest.length) {
          longest = new mutable.MutableList()
          longest ++= current
        }
      } else {
        current = new mutable.MutableList()
      }
    })

    longest.foreach(println)
    /*
    Morph(明治,名詞,固有名詞,明治)
    Morph(三,名詞,数,三)
    Morph(十,名詞,数,十)
    Morph(八,名詞,数,八)
    Morph(年,名詞,接尾,年)
    Morph(何,名詞,数,何)
    Morph(月,名詞,一般,月)
    Morph(何,名詞,数,何)
    Morph(日,名詞,接尾,日)
    Morph(戸締り,名詞,サ変接続,戸締り)
     */
  }

  /**
    * 文章中に出現する単語とその出現頻度を求め，出現頻度の高い順に並べよ． (36)
    * 出現頻度が高い10語とその出現頻度をグラフ（例えば棒グラフなど）で表示せよ． (37)
    */
  test("36. 単語の出現頻度, 37. 頻度上位10語") {
    morphs.groupBy(m => m.base)
      .toSeq
      .map(e => (e._1, e._2.length))
      .sortBy(e => e._2)
      .reverse
      .foreach(println)
    /*
    (*,11117)
    (の,9192)
    (。,7486)
    (て,6846)
    (、,6773)
    (は,6422)
    (に,6238)
    (を,6067)
    (だ,5975)
    (と,5506)
    ...
     */
  }

  test("") {

  }
}