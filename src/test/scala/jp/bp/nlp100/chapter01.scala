package jp.bp.nlp100

package jp.bp.nlp100


import org.scalatest.{FunSuite, Matchers}

import scala.collection.mutable


import scala.util.Random

/**
  * Created by shuhei on 17/10/19.
  */

class Chapter01Suite extends FunSuite with Matchers {

  /**
    * 文字列"stressed"の文字を逆に（末尾から先頭に向かって）並べた文字列を得よ．
    */
  test("00. 文字列の逆順") {
    assert("stressed".reverse == "desserts")
  }

  /**
    * 「パタトクカシーー」という文字列の1,3,5,7文字目を取り出して連結した文字列を得よ．
    */
  test("01. 「パタトクカシーー」") {
    val ans = "パタトクカシーー".zipWithIndex
       .collect { case (c, i) if i % 2 == 0 => c }
       .mkString
    assert(ans == "パトカー")
  }

  /**
    * 「パトカー」＋「タクシー」の文字を先頭から交互に連結して文字列「パタトクカシーー」を得よ．
    */
  test("02. 「パトカー」＋「タクシー」＝「パタトクカシーー」") {
    val ans = ("パトカー" zip "タクシー")
      .map { case (s1, s2) => s"$s1$s2" }
      .mkString
    assert(ans == "パタトクカシーー")
  }

  /**
    * "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
    * という文を単語に分解し，各単語の（アルファベットの）文字数を先頭から出現順に並べたリストを作成せよ．
    */
  test("03. 円周率") {
    val ans = "Now I need a drink, alcoholic of course, after the heavy lectures involving quantum mechanics."
      .replaceAll("[^a-zA-Z ]", "")
      .split(" ")
      .map(s => s.length)
      .toList
    assert(ans == List(3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7, 9))
  }

  /**
    * "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
    * という文を単語に分解し，1, 5, 6, 7, 8, 9, 15, 16, 19番目の単語は先頭の1文字，それ以外の単語は先頭に2文字を取り出し，
    * 取り出した文字列から単語の位置（先頭から何番目の単語か）への連想配列（辞書型もしくはマップ型）を作成せよ．
    */
  test("04. 元素記号") {
    val indice = List(1, 5, 6, 7, 8, 9, 15, 16, 19)
    val ans = "Hi He Lied Because Boron Could Not Oxidize Fluorine. New Nations Might Also Sign Peace Security Clause. Arthur King Can."
      .split(" ")
      .zipWithIndex
      .map {
        case (word, i) if indice contains i + 1 => (word(0).toString, i + 1)
        case (word, i) => (word.substring(0, 2), i + 1)
      }
      .toMap

    // 12は本当はMgだけど問題文の文字列だとMiになっちゃう
    val expected = Map(
      "H" -> 1, "He" -> 2, "Li" -> 3, "Be" -> 4, "B" -> 5,
      "C" -> 6, "N" -> 7, "O" -> 8, "F" -> 9, "Ne" -> 10,
      "Na" -> 11, "Mi" -> 12, "Al" -> 13, "Si" -> 14, "P" -> 15,
      "S" -> 16, "Cl" -> 17, "Ar" -> 18, "K" -> 19, "Ca" -> 20)
    // TODO Mapの上手い比較
    assert(ans.size == expected.size)
    ans.keys.foreach(k => assert(ans.get(k) == expected.get(k)))
  }

  /**
    * 与えられたシーケンス（文字列やリストなど）からn-gramを作る関数を作成せよ．
    * この関数を用い，"I am an NLPer"という文から単語bi-gram，文字bi-gramを得よ．
    */
  test("05. n-gram") {
    val str = "I am an NLPer"
    val wordBiGram = Chapter01.ngram(2)(str.split(" "))
    val charBiGram = Chapter01.ngram(2)(str)

    // TODO ここでtoListとかしなくて済むようにしたい
    assert(wordBiGram.toList == List(List("I", "am"), List("am", "an"), List("an", "NLPer")))
    assert(charBiGram.map(c => c.mkString).toList == List("I ", " a", "am", "m ", " a", "an",
        "n ", " N", "NL", "LP", "Pe", "er"))
  }

  /**
    * "paraparaparadise"と"paragraph"に含まれる文字bi-gramの集合を，それぞれ, XとYとして求め，XとYの和集合，積集合，差集合を求めよ．
    * さらに，'se'というbi-gramがXおよびYに含まれるかどうかを調べよ．
    */
  test("06. 集合") {
    val x: Set[String] = Chapter01.ngram(2)("paraparaparadise")
      .map(c => c.mkString).toSet
    val y: Set[String] = Chapter01.ngram(2)("paragraph")
      .map(c => c.mkString).toSet

    val union: Set[String] = x.union(y)
    val intersection: Set[String] = x.intersect(y)
    val difference: Set[String] = x.diff(y)

    assert(union == Set("se", "ar", "is", "pa", "ap", "di", "ra", "ad", "gr", "ag", "ph"))
    assert(intersection == Set("ar", "pa", "ap", "ra"))
    assert(difference == Set("se", "is", "di", "ad"))
  }

  /**
    * 引数x, y, zを受け取り「x時のyはz」という文字列を返す関数を実装せよ．
    * さらに，x=12, y="気温", z=22.4として，実行結果を確認せよ．
    */
  test("07. テンプレートによる文生成") {
    val str: String = Chapter01.template(12, "気温", 22.4)
    assert(str == "12時の気温は22.4")
  }

  /**
    * 与えられた文字列の各文字を，以下の仕様で変換する関数cipherを実装せよ．
    *  - 英小文字ならば(219 - 文字コード)の文字に置換
    *  - その他の文字はそのまま出力
    * この関数を用い，英語のメッセージを暗号化・復号化せよ．
    */
  test("08. 暗号文") {
    val cipherd = Chapter01.cipher("Hello, World!")
    assert(cipherd == "Hvool, Wliow!")

    val decipherd = Chapter01.cipher(cipherd)
    assert(decipherd == "Hello, World!")
  }

  /**
    * スペースで区切られた単語列に対して，各単語の先頭と末尾の文字は残し，
    * それ以外の文字の順序をランダムに並び替えるプログラムを作成せよ．
    * ただし，長さが４以下の単語は並び替えないこととする．
    * 適当な英語の文（例えば"I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."）を与え，
    * その実行結果を確認せよ．
    */
  test("09. Typoglycemia") {
    val str = "I couldn't believe that I could actually understand what I was reading : the phenomenal power of the human mind ."
    Random.setSeed(1L)

    val expected = "I cnlou'dt beielve that I cloud allacuty urtedsannd what I was reidang : the peomahnenl power of the hamun mind ."
    assert(Chapter01.typoglycemia(str) == expected)
  }
}