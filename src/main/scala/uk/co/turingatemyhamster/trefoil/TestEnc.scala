package uk.co.turingatemyhamster.trefoil

import Predef.{ArrowAssoc => _, Ensuring => _, _}

import cats.data.{Const, Prod}

/**
  *
  *
  * @author Matthew Pocock
  */
object TestEnc {

  def main(args: Array[String]): Unit = {
    println("Hi Mum")
//
//    def rainbowKb[R[_]](implicit R: RdfTagless[R]) = {
//      import R._
//      import R.implicits._
//
//      val rod = iri("http://test.org/test#rod")
//      val jane = iri("http://test.org/test#jane")
//      val freddie = iri("http://test.org/test#freddie")
//
//      val friend = iri("http://foaf.org/foaf1.1#friend")
//
//      val rodJaneFriends = triple(rod, friend, jane)
//      val janeFreddieFriends = triple(jane, friend, freddie)
//
//      graph(rodJaneFriends, janeFreddieFriends)
//    }
//
//    val objects = Eval get rainbowKb[cats.Id]
//    println(s"Object model:\n$objects")
//
//    val prettyPrinted = Eval get rainbowKb[Const[List[String], ?]]
//    println(s"Pretty Print:\n$prettyPrinted")
//
//    val longEncoded = Eval get rainbowKb[Encoding]
//    println(s"Long encoding:\n$longEncoded")
//
//    type t[X] = Prod[cats.Id, Encoding, X] // have to use this -- see productEncoding for a failing case
//    val someEncoding = Eval.get(rainbowKb[t])
//    println(s"Combined encoding:\n$someEncoding")

    // using the Product type directly fails -- see t[X] above for type-alias that works(ish)
//    val productEncoding = Eval.get(rainbowKb[Prod[cats.Id, Encoding, ?]](RdfTagless.fromAux2[Prod, cats.Id, Encoding]))
//    println(s"Combined encoding:\n$productEncoding")

    // fixme: Doesn't work - ?Rep[?S] isn't currying at all here for the nested case
//    type u[X] = Prod[Const[List[String], ?], t, X]
//    val allEncoding = Eval.get(rainbowKb[u])
//    println(s"Combined encoding:\n$allEncoding")
  }
}
