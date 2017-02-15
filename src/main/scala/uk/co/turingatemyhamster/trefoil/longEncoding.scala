package uk.co.turingatemyhamster.trefoil

import scorex.crypto.hash.Blake2b256

case class LongEncodedTriple(subject: Long, predicate: Long, `object`: Long)
case class LongEncodedQuad(subject: Long, predicate: Long, `object`: Long, graph: Long)

trait EncodedAs[A, B] {
  def apply(lea: Encoding[A]): B
  def cast[A1](lea: Encoding[A]): Encoding[A1]
}

object EncodedAs {
  def apply[A, B](a: Encoding[A])(implicit G: EncodedAs[A, B]): B = G(a)
  final def cast[A1, A2, B](a: Encoding[A1])(implicit G: EncodedAs[A1, B]): Encoding.Aux[A2, B] =
    a.asInstanceOf[Encoding.Aux[A1, B]].cast[A2]

  def leg[A, B]: EncodedAs[A, B] = new EncodedAs[A, B] {
    override def apply(lea: Encoding[A]): B = lea.apply.asInstanceOf[B] // eugh!
    override def cast[A1](lea: Encoding[A]): Encoding[A1] = lea.cast[A1]
  }

  // The mapping from DSL types to implementation types
  implicit val Iri: EncodedAs[RdfCore.Iri, Long] = leg
  implicit val BNode: EncodedAs[RdfCore.BNode, Long] = leg
  implicit val Literal: EncodedAs[RdfCore.Literal, Long] = leg
  implicit val Subject: EncodedAs[RdfCore.Subject, Long] = leg
  implicit val Predicate: EncodedAs[RdfCore.Predicate, Long] = leg
  implicit val Object: EncodedAs[RdfCore.Object, Long] = leg
  implicit val GraphName: EncodedAs[RdfCore.GraphName, Long] = leg
  implicit val Triple: EncodedAs[RdfCore.Triple, LongEncodedTriple] = leg
  implicit val Graph: EncodedAs[RdfCore.Graph, Set[LongEncodedTriple]] = leg
}

trait Encoding[A] {
  type To
  def apply: To
  final def cast[B]: Encoding.Aux[B, To] = this.asInstanceOf[Encoding.Aux[B, To]]
}

object Encoding {

  implicit def evalEncoding[A, B](implicit A: EncodedAs[A, B]): Eval.AsAux[Encoding, A, B] = new Eval.As[Encoding, A] {
    type T = B

    override def apply(s: Encoding[A]): B = A(s)
  }

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
      EncodedAs cast iri

    override def bNodeAsSubject(bNode: Encoding[RdfCore.BNode]): Aux[RdfCore.Subject, Long] =
      EncodedAs cast bNode

    override def iriAsPredicate(iri: Encoding[RdfCore.Iri]): Aux[RdfCore.Predicate, Long] =
      EncodedAs cast iri

    override def iriAsObject(iri: Encoding[RdfCore.Iri]): Aux[RdfCore.Object, Long] =
      EncodedAs cast iri

    override def bNodeAsObject(bNode: Encoding[RdfCore.BNode]): Aux[RdfCore.Object, Long] =
      EncodedAs cast bNode

    override def literalAsObject(literal: Encoding[RdfCore.Literal]): Aux[RdfCore.Object, Long] =
      EncodedAs cast literal

    override def iriAsGraphName(iri: Encoding[RdfCore.Iri]): Aux[RdfCore.GraphName, Long] =
      EncodedAs cast iri

    override def bNodeAsGraphName(bNode: Encoding[RdfCore.BNode]): Aux[RdfCore.GraphName, Long] =
      EncodedAs cast bNode

    override def triple(subject: Encoding[RdfCore.Subject],
                        predicate: Encoding[RdfCore.Predicate],
                        `object`: Encoding[RdfCore.Object]): Aux[RdfCore.Triple, LongEncodedTriple] =
      Encoding encode LongEncodedTriple(EncodedAs(subject), EncodedAs(predicate), EncodedAs(`object`))

    override def quad(subject: Encoding[RdfCore.Subject],
                      predicate: Encoding[RdfCore.Predicate],
                      `object`: Encoding[RdfCore.Object],
                      graph: Encoding[RdfCore.GraphName]): Aux[RdfCore.Quad, LongEncodedQuad] =
      Encoding encode LongEncodedQuad(EncodedAs(subject), EncodedAs(predicate), EncodedAs(`object`), EncodedAs(graph))

    // not sure how to implement these yet
    override def graph(triples: Encoding[RdfCore.Triple]*): Aux[RdfCore.Graph, Set[LongEncodedTriple]] =
      Encoding encode Set(triples map (t => EncodedAs(t)) :_*)

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