package uk.co.turingatemyhamster.trefoil

import scorex.crypto.hash.Blake2b256

case class LongEncodedTriple(subject: Long, predicate: Long, `object`: Long)
case class LongEncodedQuad(subject: Long, predicate: Long, `object`: Long, graph: Long)

trait Encoding[A] {
  type To
  def apply: To
  final def cast[B]: Encoding.Aux[B, To] = this.asInstanceOf[Encoding.Aux[B, To]]
}

trait GetEncoding[A, B] {
  def apply(lea: Encoding[A]): B
  def cast[A1](lea: Encoding[A]): Encoding[A1]
}

object GetEncoding {
  def apply[A, B](a: Encoding[A])(implicit G: GetEncoding[A, B]): B = G(a)
  final def cast[A1, A2, B](a: Encoding[A1])(implicit G: GetEncoding[A1, B]): Encoding.Aux[A2, B] =
    a.asInstanceOf[Encoding.Aux[A1, B]].cast[A2]

  def leg[A, B]: GetEncoding[A, B] = new GetEncoding[A, B] {
    override def apply(lea: Encoding[A]): B = lea.apply.asInstanceOf[B] // eugh!
    override def cast[A1](lea: Encoding[A]): Encoding[A1] = lea.cast[A1]
  }

  // The mapping from DSL types to implementation types
  implicit val Iri: GetEncoding[RdfCore.Iri, Long] = leg
  implicit val BNode: GetEncoding[RdfCore.BNode, Long] = leg
  implicit val Literal: GetEncoding[RdfCore.Literal, Long] = leg
  implicit val Subject: GetEncoding[RdfCore.Subject, Long] = leg
  implicit val Predicate: GetEncoding[RdfCore.Predicate, Long] = leg
  implicit val Object: GetEncoding[RdfCore.Object, Long] = leg
  implicit val GraphName: GetEncoding[RdfCore.GraphName, Long] = leg
  implicit val Triple: GetEncoding[RdfCore.Triple, LongEncodedTriple] = leg
}

object Encoding {

  type Aux[T, To0] = Encoding[T] {
    type To = To0
  }

  def encode[T, A](a: A): Aux[T, A] = new Encoding[T] {
    type To = A
    def apply: A = a
  }

  def longTagless(enc: String => Long): RdfTagless[Encoding] = new RdfTagless[Encoding] {
    override def iri(toString: String): Aux[RdfCore.Iri, Long] =
      Encoding encode enc(s"<$toString>")

    override def bNode(localName: String): Aux[RdfCore.BNode, Long] =
      Encoding encode enc(s"_:$localName")

    override def literal(lexicalValue: String,
                         languageCode: Option[String],
                         xsdType: Option[String]): Aux[RdfCore.Literal, Long] =
      Encoding encode enc(s""""$lexicalValue\"${
        languageCode.fold("")("@@" + _)
      }${
        xsdType.fold("")("^^" + _)
      }""")

    override def iriAsSubject(iri: Encoding[RdfCore.Iri]): Aux[RdfCore.Subject, Long] =
      GetEncoding cast iri

    override def bNodeAsSubject(bNode: Encoding[RdfCore.BNode]): Aux[RdfCore.Subject, Long] =
      GetEncoding cast bNode

    override def iriAsPredicate(iri: Encoding[RdfCore.Iri]): Aux[RdfCore.Predicate, Long] =
      GetEncoding cast iri

    override def iriAsObject(iri: Encoding[RdfCore.Iri]): Aux[RdfCore.Object, Long] =
      GetEncoding cast iri

    override def bNodeAsObject(bNode: Encoding[RdfCore.BNode]): Aux[RdfCore.Object, Long] =
      GetEncoding cast bNode

    override def literalAsObject(literal: Encoding[RdfCore.Literal]): Aux[RdfCore.Object, Long] =
      GetEncoding cast literal

    override def iriAsGraphName(iri: Encoding[RdfCore.Iri]): Aux[RdfCore.GraphName, Long] =
      GetEncoding cast iri

    override def bNodeAsGraphName(bNode: Encoding[RdfCore.BNode]): Aux[RdfCore.GraphName, Long] =
      GetEncoding cast bNode

    override def triple(subject: Encoding[RdfCore.Subject],
                        predicate: Encoding[RdfCore.Predicate],
                        `object`: Encoding[RdfCore.Object]): Aux[RdfCore.Triple, LongEncodedTriple] =
      Encoding encode LongEncodedTriple(GetEncoding(subject), GetEncoding(predicate), GetEncoding(`object`))

    override def quad(subject: Encoding[RdfCore.Subject],
                      predicate: Encoding[RdfCore.Predicate],
                      `object`: Encoding[RdfCore.Object],
                      graph: Encoding[RdfCore.GraphName]): Aux[RdfCore.Quad, LongEncodedQuad] =
      Encoding encode LongEncodedQuad(GetEncoding(subject), GetEncoding(predicate), GetEncoding(`object`), GetEncoding(graph))

    // not sure how to implement these yet
    override def graph(triples: Encoding[RdfCore.Triple]*): Aux[RdfCore.Graph, Set[LongEncodedTriple]] =
      Encoding encode Set(triples map (t => GetEncoding(t)) :_*)

    override def dataset(defaultGraph: Encoding[RdfCore.Graph],
                         namedGraphs: Map[Encoding[RdfCore.GraphName], Encoding[RdfCore.Graph]]):
    Encoding[RdfCore.Dataset] = ???
  }

  implicit val blake2bTagless: RdfTagless[Encoding] = longTagless { (s: String) =>
    val bytes = Blake2b256.hash(s)
    val bi = BigInt(bytes)
    // overflow!!!
    bi.longValue()
  }
}