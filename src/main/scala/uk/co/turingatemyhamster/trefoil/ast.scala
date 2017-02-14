package uk.co.turingatemyhamster.trefoil

import cats.Id
import cats.data.Const

object RdfCore {
  sealed trait Subject
  sealed trait Predicate
  sealed trait Object
  sealed trait GraphName

  case class Iri(override val toString: String) extends Subject with Predicate with Object with GraphName
  case class BNode(localName: String) extends Subject with Object with GraphName
  // at most one of lc and xt
  case class Literal(lexicalValue: String, languageCode: Option[String], xsdType: Option[String]) extends Object

  case class Triple(subject: Subject, predicate: Predicate, `object`: Object)
  case class Quad(subject: Subject, predicate: Predicate, `object`: Object, graphName: GraphName)

  case class Graph(triples: Set[Triple])
  case class Dataset(defaultGraph: Graph, namedGraphs: Map[GraphName, Graph])
}


//trait Eval[T] {
//  type Rep[_]
//  def eval[S](s: RdfTagless[Rep] => Rep[S])(implicit S: RdfTagless[Rep]): T
//}
//
//object Eval {
//  type Aux[T, Rep0[_]] = Eval[T] {
//    type Rep[_] = Rep0[_]
//  }
//
//  def apply[T, Rep[_]](implicit T: Aux[T, Rep]): Aux[T, Rep] = T
//
//  implicit object evalAppendableToString extends Eval[String] {
//    type Rep[_] = RdfTagless.appendable[_]
////    override def apply[S](s: RdfTagless[RdfTagless.appendable] => RdfTagless.appendable[S])(implicit S: RdfTagless[RdfTagless.appendable]): String = s(S).getConst.mkString
//    override def eval[S](s: (RdfTagless[appendable]) => appendable[S])
//                        (implicit S: RdfTagless[appendable]): String = s(S).getConst.mkString
//  }
//}

import RdfCore._

trait RdfTagless[Rep[_]] {
  // constructors
  def iri(toString: String): Rep[Iri]
  def bNode(localName: String): Rep[BNode]
  def literal(lexicalValue: String, languageCode: Option[String], xsdType: Option[String]): Rep[Literal]

  // casts
  def iriAsSubject(iri: Rep[Iri]): Rep[Subject]
  def bNodeAsSubject(bNode: Rep[BNode]): Rep[Subject]

  def iriAsPredicate(iri: Rep[Iri]): Rep[Predicate]

  def iriAsObject(iri: Rep[Iri]): Rep[Object]
  def bNodeAsObject(bNode: Rep[BNode]): Rep[Object]
  def literalAsObject(literal: Rep[Literal]): Rep[Object]

  def iriAsGraphName(iri: Rep[Iri]): Rep[GraphName]
  def bNodeAsGraphName(bNode: Rep[BNode]): Rep[GraphName] // sparql 1.1 may not allow bnodes as graph names

  // builders
  def triple(subject: Rep[Subject], predicate: Rep[Predicate], `object`: Rep[Object]): Rep[Triple]
  def quad(subject: Rep[Subject], predicate: Rep[Predicate], `object`: Rep[Object], graph: Rep[GraphName]): Rep[Quad]
  def graph(triples: Rep[Triple]*): Rep[Graph]
  def dataset(defaultGraph: Rep[Graph], namedGraphs: Map[Rep[GraphName], Rep[Graph]]): Rep[Dataset]

  object implicits {
    implicit def _iriAsSubject(iri: Rep[Iri]): Rep[Subject] = iriAsSubject(iri)
    implicit def _bNodeAsSubject(bNode: Rep[BNode]): Rep[Subject] = bNodeAsSubject(bNode)

    implicit def _iriAsPredicate(iri: Rep[Iri]): Rep[Predicate] = iriAsPredicate(iri)

    implicit def _iriAsObject(iri: Rep[Iri]): Rep[Object] = iriAsObject(iri)
    implicit def _bNodeAsObject(bNode: Rep[BNode]): Rep[Object] = bNodeAsObject(bNode)
    implicit def _literalAsObject(literal: Rep[Literal]): Rep[Object] = literalAsObject(literal)

    implicit def _iriAsGraphName(iri: Rep[Iri]): Rep[GraphName] = iriAsGraphName(iri)
    implicit def _bNodeAsGraphName(bNode: Rep[BNode]): Rep[GraphName] = bNodeAsGraphName(bNode) // sparql 1.1 may not allow bnodes as graph names
  }
}

sealed trait RdfProperties[Rep[_]] {

  // properties
  def subjectOfTriple(triple: Rep[Triple]): Rep[Subject]
  def predicateOfTriple(triple: Rep[Triple]): Rep[Predicate]
  def objectOfTriple(triple: Rep[Triple]): Rep[Object]

  def subjectOfQuad(quad: Rep[Quad]): Rep[Subject]
  def predicateOfQuad(quad: Rep[Quad]): Rep[Predicate]
  def objectOfQuad(quad: Rep[Quad]): Rep[Object]
  def graphOfQuad(quad: Rep[Quad]): Rep[GraphName]

}

object RdfTagless /*extends RdfTagless_LowPriority*/ {

  implicit object IdTagless extends RdfTagless[Id] {
    override def iri(toString: String): Id[Iri] = Iri(toString)

    override def bNode(localName: String): Id[BNode] = BNode(localName)

    override def literal(lexicalValue: String,
                         languageCode: Option[String],
                         xsdType: Option[String]): Id[Literal] = Literal(lexicalValue, languageCode, xsdType)

    override def iriAsSubject(iri: Id[Iri]): Id[Subject] = iri

    override def bNodeAsSubject(bNode: Id[BNode]): Id[Subject] = bNode

    override def iriAsPredicate(iri: Id[Iri]): Id[Predicate] = iri

    override def iriAsObject(iri: Id[Iri]): Id[Object] = iri

    override def bNodeAsObject(bNode: Id[BNode]): Id[Object] = bNode

    override def literalAsObject(literal: Id[Literal]): Id[Object] = literal

    override def iriAsGraphName(iri: Id[Iri]): Id[GraphName] = iri

    override def bNodeAsGraphName(bNode: Id[BNode]): Id[GraphName] = bNode

    override def triple(subject: Id[Subject],
                        predicate: Id[Predicate],
                        `object`: Id[Object]): Id[Triple] = Triple(subject, predicate, `object`)

    override def quad(subject: Id[Subject],
                      predicate: Id[Predicate],
                      `object`: Id[Object],
                      graph: Id[GraphName]): Id[Quad] = Quad(subject, predicate, `object`, graph)

    override def graph(triples: Id[Triple]*): Id[Graph] = Graph(Set(triples :_*))

    override def dataset(defaultGraph: Id[Graph],
                         namedGraphs: Map[Id[GraphName], Id[Graph]]): Id[Dataset] = Dataset(defaultGraph, namedGraphs)
  }

  implicit object IdProperties extends RdfProperties[Id] {

    override def subjectOfTriple(triple: Id[Triple]): Id[Subject] = triple.subject

    override def predicateOfTriple(triple: Id[Triple]): Id[Predicate] = triple.predicate

    override def objectOfTriple(triple: Id[Triple]): Id[Object] = triple.`object`

    override def subjectOfQuad(quad: Id[Quad]): Id[Subject] = quad.subject

    override def predicateOfQuad(quad: Id[Quad]): Id[Predicate] = quad.predicate

    override def objectOfQuad(quad: Id[Quad]): Id[Object] = quad.`object`

    override def graphOfQuad(quad: Id[Quad]): Id[GraphName] = quad.graphName

  }

  type appendable[B] = Const[List[String], B]

  implicit object PrettyPrintTagless extends RdfTagless[appendable] {
    override def iri(toString: String): appendable[Iri] =
      Const(s"<$toString>" :: Nil)

    override def bNode(localName: String): appendable[BNode] =
      Const(s"_:$localName" :: Nil)

    override def literal(lexicalValue: String,
                         languageCode: Option[String],
                         xsdType: Option[String]): appendable[Literal] =
      Const("\"" :: lexicalValue :: "\"" :: languageCode.map("@@" + _).to[List] ::: xsdType.map("^^" + _).to[List])

    override def iriAsSubject(iri: appendable[Iri]): appendable[Subject] =
      iri.retag[Subject]

    override def bNodeAsSubject(bNode: appendable[BNode]): appendable[Subject] =
      bNode.retag[Subject]

    override def iriAsPredicate(iri: appendable[Iri]): appendable[Predicate] =
      iri.retag[Predicate]

    override def iriAsObject(iri: appendable[Iri]): appendable[Object] =
      iri.retag[Object]

    override def bNodeAsObject(bNode: appendable[BNode]): appendable[Object] =
      bNode.retag[Object]

    override def literalAsObject(literal: appendable[Literal]): appendable[Object] =
      literal.retag[Object]

    override def iriAsGraphName(iri: appendable[Iri]): appendable[GraphName] =
      iri.retag[GraphName]

    override def bNodeAsGraphName(bNode: appendable[BNode]): appendable[GraphName] =
      bNode.retag[GraphName]

    override def triple(subject: appendable[Subject],
                        predicate: appendable[Predicate],
                        `object`: appendable[Object]): appendable[Triple] =
      Const(subject.getConst ::: " " :: predicate.getConst ::: " " :: `object`.getConst ::: " ." :: Nil)

    // code smell - this looks specific to the sparql 1.1 syntax and differs from e.g. n-triples/n-quads
    override def quad(subject: appendable[Subject],
                      predicate: appendable[Predicate],
                      `object`: appendable[Object],
                      graph: appendable[GraphName]): appendable[Quad] =
      Const(
        "GRAPH " :: graph.getConst ::: " { " ::
          subject.getConst ::: " " :: predicate.getConst ::: " " :: `object`.getConst ::: " } ." :: Nil)

    override def graph(triples: appendable[Triple]*): appendable[Graph] =
      Const(triples.to[List].flatMap(_.getConst ::: "\n" :: Nil))

    // code smell - there's not an obvious way to render this to text
    override def dataset(defaultGraph: appendable[Graph],
                         namedGraphs: Map[appendable[GraphName], appendable[Graph]]): appendable[Dataset] = ???
  }
}

//trait RdfTagless_LowPriority {
//
//  type both[A[_], B[_]] = {
//    type of[T] = (A[T], B[T])
//  }
//
//  implicit def bothTagless[R1[_], R2[_]](t1: RdfTagless[R1], t2: RdfTagless[R2])
//  : RdfTagless[both[R1, R2]#of] = new RdfTagless[both[R1, R2]#of] {
//    override def iri(toString: String): (R1[Iri], R2[Iri]) =
//      t1.iri(toString) -> t2.iri(toString)
//
//    override def bNode(localName: String): (R1[BNode], R2[BNode]) =
//      t1.bNode(localName) -> t2.bNode(localName)
//
//    override def literal(lexicalValue: String,
//                         languageCode: Option[String],
//                         xsdType: Option[String]): (R1[Literal], R2[Literal]) =
//      t1.literal(lexicalValue, languageCode, xsdType) -> t2.literal(lexicalValue, languageCode, xsdType)
//
//    override def iriAsSubject(iri: (R1[Iri], R2[Iri])): (R1[Subject], R2[Subject]) =
//      t1.iriAsSubject(iri._1) -> t2.iriAsSubject(iri._2)
//
//    override def bNodeAsSubject(bNode: (R1[BNode], R2[BNode])): (R1[Subject], R2[Subject]) =
//      t1.bNodeAsSubject(bNode._1) -> t2.bNodeAsSubject(bNode._2)
//
//    override def iriAsPredicate(iri: (R1[Iri], R2[Iri])): (R1[Predicate], R2[Predicate]) =
//      t1.iriAsPredicate(iri._1) -> t2.iriAsPredicate(iri._2)
//
//    override def iriAsObject(iri: (R1[Iri], R2[Iri])): (R1[Object], R2[Object]) =
//      t1.iriAsObject(iri._1) -> t2.iriAsObject(iri._2)
//
//    override def bNodeAsObject(bNode: (R1[BNode], R2[BNode])): (R1[Object], R2[Object]) =
//      t1.bNodeAsObject(bNode._1) -> t2.bNodeAsObject(bNode._2)
//
//    override def literalAsObject(literal: (R1[Literal], R2[Literal])): (R1[Object], R2[Object]) =
//      t1.literalAsObject(literal._1) -> t2.literalAsObject(literal._2)
//
//    override def iriAsGraphName(iri: (R1[Iri], R2[Iri])): (R1[GraphName], R2[GraphName]) =
//      t1.iriAsGraphName(iri._1) -> t2.iriAsGraphName(iri._2)
//
//    override def bNodeAsGraphName(bNode: (R1[BNode], R2[BNode])): (R1[GraphName], R2[GraphName]) =
//      t1.bNodeAsGraphName(bNode._1) -> t2.bNodeAsGraphName(bNode._2)
//
//    override def triple(subject: (R1[Subject], R2[Subject]),
//                        predicate: (R1[Predicate], R2[Predicate]),
//                        `object`: (R1[Object], R2[Object])): (R1[Triple], R2[Triple]) =
//      t1.triple(subject._1, predicate._1, `object`._1) -> t2.triple(subject._2, predicate._2, `object`._2)
//
//    override def quad(subject: (R1[Subject], R2[Subject]),
//                      predicate: (R1[Predicate], R2[Predicate]),
//                      `object`: (R1[Object], R2[Object]),
//                      graph: (R1[GraphName], R2[GraphName])): (R1[Quad], R2[Quad]) =
//      t1.quad(subject._1, predicate._1, `object`._1, graph._1) -> t2.quad(subject._2, predicate._2, `object`._2, graph._2)
//
//    override def graph(triples: (R1[Triple], R2[Triple])*): (R1[Graph], R2[Graph]) =
//      t1.graph(triples.map(_._1) :_*) -> t2.graph(triples.map(_._2) :_*)
//
//    override def dataset(defaultGraph: (R1[Graph], R2[Graph]),
//                         namedGraphs: Map[(R1[GraphName], R2[GraphName]), (R1[Graph], R2[Graph])]): (R1[Dataset], R2[Dataset]) =
//      t1.dataset(defaultGraph._1, namedGraphs map { case (k, v) => k._1 -> v._1 }) -> t2.dataset(defaultGraph._2, namedGraphs map { case (k, v) => k._2 -> v._2 })
//  }
//
//  implicit def bothProperties[R1[_], R2[_]](t1: RdfProperties[R1], t2: RdfProperties[R2])
//    : RdfProperties[both[R1, R2]#of] = new RdfProperties[both[R1, R2]#of] {
//
//    override def subjectOfTriple(triple: (R1[Triple], R2[Triple])): (R1[Subject], R2[Subject]) =
//      t1.subjectOfTriple(triple._1) -> t2.subjectOfTriple(triple._2)
//
//    override def predicateOfTriple(triple: (R1[Triple], R2[Triple])): (R1[Predicate], R2[Predicate]) =
//      t1.predicateOfTriple(triple._1) -> t2.predicateOfTriple(triple._2)
//
//    override def objectOfTriple(triple: (R1[Triple], R2[Triple])): (R1[Object], R2[Object]) =
//      t1.objectOfTriple(triple._1) -> t2.objectOfTriple(triple._2)
//
//    override def subjectOfQuad(quad: (R1[Quad], R2[Quad])): (R1[Subject], R2[Subject]) =
//      t1.subjectOfQuad(quad._1) -> t2.subjectOfQuad(quad._2)
//
//    override def predicateOfQuad(quad: (R1[Quad], R2[Quad])): (R1[Predicate], R2[Predicate]) =
//      t1.predicateOfQuad(quad._1) -> t2.predicateOfQuad(quad._2)
//
//    override def objectOfQuad(quad: (R1[Quad], R2[Quad])): (R1[Object], R2[Object]) =
//      t1.objectOfQuad(quad._1) -> t2.objectOfQuad(quad._2)
//
//    override def graphOfQuad(quad: (R1[Quad], R2[Quad])): (R1[GraphName], R2[GraphName]) =
//      t1.graphOfQuad(quad._1) -> t2.graphOfQuad(quad._2)
//
//  }
//
//}