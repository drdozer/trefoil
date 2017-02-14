package uk.co.turingatemyhamster.trefoil

/**
  *
  *
  * @author Matthew Pocock
  */
object TestEnc {
  def main(args: Array[String]): Unit = {
    println("Hi Mum")

    def rainbowKb[R[_]](implicit R: RdfTagless[R]) = {
      import R._
      import R.implicits._

      val rod = iri("http://test.org/test#rod")
      val jane = iri("http://test.org/test#jane")
      val freddie = iri("http://test.org/test#freddie")

      val friend = iri("http://foaf.org/foaf1.1#friend")

      val rodJaneFriends = triple(rod, friend, jane)
      val janeFreddieFriends = triple(jane, friend, freddie)

      graph(rodJaneFriends, janeFreddieFriends)
    }

    val objects = rainbowKb[cats.Id]
    println(s"Object model:\n$objects")

    val prettyPrinted = rainbowKb[RdfTagless.appendable].getConst.mkString
    println(s"Pretty Print:\n$prettyPrinted")

    val longEncoded = rainbowKb[Encoding].apply
    println(s"Long encoding:\n$longEncoded")


  }
}
