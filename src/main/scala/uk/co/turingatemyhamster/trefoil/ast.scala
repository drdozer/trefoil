package uk.co.turingatemyhamster.trefoil

import cats.Id
import cats.data.{Const, Prod}

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

/** Replay a type `T` via a representation into a value `U`. */

trait Replay[T] {
  def at[Rep[_]](t: T)(implicit r: RdfTagless[Rep]): Rep[T]
}

object Replay {
  def apply[T](t: T)(implicit T: Replay[T]) = At(t)

  case class At[T](t: T)(implicit T: Replay[T]) {
    def at[Rep[_]](implicit r: RdfTagless[Rep]): Rep[T] = T.at(t)(r)
    def evalAt[Rep[_]](implicit r: RdfTagless[Rep], E: Eval.As[Rep, T]): E.T = E(T.at(t)(r))
  }

  implicit object replayIri extends Replay[RdfCore.Iri] {
    override def at[Rep[_]](t: RdfCore.Iri)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Iri] = r.iri(t.toString)
  }

  implicit object replayBNode extends Replay[RdfCore.BNode] {
    override def at[Rep[_]](t: RdfCore.BNode)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.BNode] = r.bNode(t.localName)
  }

  implicit object replayLiteral extends Replay[RdfCore.Literal] {
    override def at[Rep[_]](t: RdfCore.Literal)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Literal] = r.literal(t.lexicalValue, t.languageCode, t.xsdType)
  }

  implicit object replaySubject extends Replay[RdfCore.Subject] {
    override def at[Rep[_]](t: RdfCore.Subject)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Subject] = t match  {
      case iri: RdfCore.Iri => r.iriAsSubject(Replay(iri).at[Rep])
      case bNode: RdfCore.BNode => r.bNodeAsSubject(Replay(bNode).at[Rep])
    }
  }

  implicit object replayPredicate extends Replay[RdfCore.Predicate] {
    override def at[Rep[_]](t: RdfCore.Predicate)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Predicate] = t match {
      case iri: RdfCore.Iri => r.iriAsPredicate(Replay(iri).at[Rep])
    }
  }

  implicit object replayObject extends Replay[RdfCore.Object] {
    override def at[Rep[_]](t: RdfCore.Object)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Object] = t match {
      case iri: RdfCore.Iri => r.iriAsObject(Replay(iri).at[Rep])
      case bNode: RdfCore.BNode => r.bNodeAsObject(Replay(bNode).at[Rep])
      case literal: RdfCore.Literal => r.literalAsObject(Replay(literal).at[Rep])
    }
  }

  implicit object replayGraphName extends Replay[RdfCore.GraphName] {
    override def at[Rep[_]](t: RdfCore.GraphName)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.GraphName] = t match {
      case iri: RdfCore.Iri => r.iriAsGraphName(Replay(iri).at[Rep])
      case bNode: RdfCore.BNode => r.bNodeAsGraphName(Replay(bNode).at[Rep])
    }
  }

  implicit object replayTriple extends Replay[RdfCore.Triple] {
    override def at[Rep[_]](t: RdfCore.Triple)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Triple] =
      r.triple(Replay(t.subject).at[Rep], Replay(t.predicate).at[Rep], Replay(t.`object`).at[Rep])
  }

  implicit object replayQuad extends Replay[RdfCore.Quad] {
    override def at[Rep[_]](t: RdfCore.Quad)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Quad] =
      r.quad(Replay(t.subject).at[Rep], Replay(t.predicate).at[Rep], Replay(t.`object`).at[Rep], Replay(t.graphName).at[Rep])
  }

  implicit object replayGraph extends Replay[RdfCore.Graph] {
    override def at[Rep[_]](t: RdfCore.Graph)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Graph] =
      r.graph(t.triples.to[Seq] map (Replay(_).at[Rep]) :_*)
  }

  implicit object replayDataset extends Replay[RdfCore.Dataset] {
    override def at[Rep[_]](t: RdfCore.Dataset)
                           (implicit r: RdfTagless[Rep]): Rep[RdfCore.Dataset] =
    r.dataset(Replay(
      t.defaultGraph).at[Rep],
      t.namedGraphs.map { case (k, v) => Replay(k).at[Rep] -> Replay(v).at[Rep] })
  }
}

trait AsAuxUnapply[Rep[_, _], S0, S, T] {
  type R[x] = Rep[S0, x]

  def apply: Eval.AsAux[R, S, T]
}

//object AsAuxUnapply {
//  implicit def instance[Rep[_, _], S0, S, T](implicit As: Eval.AsAux[[X] => Rep[S0, X], S, T]) = new AsAuxUnapply[Rep, S0, S, T] {
//    def apply = As
//  }
//}

trait Eval[Rep[_]] {
  def eval[S, T](s: Rep[S])(implicit As: Eval.AsAux[Rep, S, T]): T
}

object Eval {
  def apply[Rep[_]](implicit Rep: Eval[Rep]): Eval[Rep] = Rep
  def get[Rep[_], S, T](s: Rep[S])(implicit As: Eval.AsAux[Rep, S, T]): T = As(s)

  // code smell - unification isn't unifying well-enough here, so the AsAuxUnapply is needed to guide type inference
  def get[Rep[_, _], S0, S, T](s: Rep[S0, S])(implicit AsU: AsAuxUnapply[Rep, S0, S, T]): T =
    AsU.apply(s)

  implicit def evalAppendableToString[S]: AsAux[[X] => Const[List[String], X], S, String] = new As[[X] => Const[List[String], X], S] {
    type T = String
    override def apply(s: Const[List[String], S]): String = s.getConst.mkString
  }

  implicit def evalId[S]: AsAux[Id, S, S] = new As[Id, S] {
    type T = S
    override def apply(s: Id[S]): S = s
  }

  implicit def evalProd[R1[_], R2[_], S, T1, T2]
  (implicit R1: AsAux[R1, S, T1], R2: AsAux[R2, S, T2])
  : AsAux[[X] => Prod[R1, R2, X], S, (T1, T2)] = new As[[X] => Prod[R1, R2, X], S] {
    type T = (T1, T2)

    override def apply(s: Prod[R1, R2, S]): (T1, T2) = (R1(s.first), R2(s.second))
  }

  trait As[Rep[_], S] {
    type T
    def apply(s: Rep[S]): T
  }

  type AsAux[Rep[_], S, T0] = As[Rep, S] {
    type T = T0
  }
}

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

<<<<<<< HEAD
object RdfTagless extends RdfTagless_LowPriority /* with RdfTagless_LowLowPriority */ {
=======
object RdfTagless extends RdfTagless_LowPriority {
>>>>>>> a5723e10ab7e181fb661f297a5681dc5aa59e796

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

<<<<<<< HEAD
  implicit object PrettyPrintTagless extends RdfTagless[[X] => Const[List[String], X]] {
=======
  implicit object PrettyPrintTagless extends RdfTagless[Const[List[String], ?]] {
>>>>>>> a5723e10ab7e181fb661f297a5681dc5aa59e796
    override def iri(toString: String): Const[List[String], Iri] =
      Const(s"<$toString>" :: Nil)

    override def bNode(localName: String): Const[List[String], BNode] =
      Const(s"_:$localName" :: Nil)

    override def literal(lexicalValue: String,
                         languageCode: Option[String],
                         xsdType: Option[String]): Const[List[String], Literal] =
      Const("\"" :: lexicalValue :: "\"" :: languageCode.map("@@" + _).to[List] ::: xsdType.map("^^" + _).to[List])

    override def iriAsSubject(iri: Const[List[String], Iri]): Const[List[String], Subject] =
      iri.retag[Subject]

    override def bNodeAsSubject(bNode: Const[List[String], BNode]): Const[List[String], Subject] =
      bNode.retag[Subject]

    override def iriAsPredicate(iri: Const[List[String], Iri]): Const[List[String], Predicate] =
      iri.retag[Predicate]

    override def iriAsObject(iri: Const[List[String], Iri]): Const[List[String], Object] =
      iri.retag[Object]

    override def bNodeAsObject(bNode: Const[List[String], BNode]): Const[List[String], Object] =
      bNode.retag[Object]

    override def literalAsObject(literal: Const[List[String], Literal]): Const[List[String], Object] =
      literal.retag[Object]

    override def iriAsGraphName(iri: Const[List[String], Iri]): Const[List[String], GraphName] =
      iri.retag[GraphName]

    override def bNodeAsGraphName(bNode: Const[List[String], BNode]): Const[List[String], GraphName] =
      bNode.retag[GraphName]

    override def triple(subject: Const[List[String], Subject],
                        predicate: Const[List[String], Predicate],
                        `object`: Const[List[String], Object]): Const[List[String], Triple] =
      Const(subject.getConst ::: " " :: predicate.getConst ::: " " :: `object`.getConst ::: " ." :: Nil)

    // code smell - this looks specific to the sparql 1.1 syntax and differs from e.g. n-triples/n-quads
    override def quad(subject: Const[List[String], Subject],
                      predicate: Const[List[String], Predicate],
                      `object`: Const[List[String], Object],
                      graph: Const[List[String], GraphName]): Const[List[String], Quad] =
      Const(
        "GRAPH " :: graph.getConst ::: " { " ::
          subject.getConst ::: " " :: predicate.getConst ::: " " :: `object`.getConst ::: " } ." :: Nil)

    override def graph(triples: Const[List[String], Triple]*): Const[List[String], Graph] =
      Const(triples.to[List].flatMap(_.getConst ::: "\n" :: Nil))

    // code smell - there's not an obvious way to render this to text
    override def dataset(defaultGraph: Const[List[String], Graph],
                         namedGraphs: Map[Const[List[String], GraphName], Const[List[String], Graph]]): Const[List[String], Dataset] = ???
  }
}

<<<<<<< HEAD
trait TaglessAux2[F[_[_], _[_], _], R1[_], R2[_]] {
  type R[T] = F[R1, R2, T]

  def apply: RdfTagless[R]
}

object TaglessAux2 {
  implicit def fromTagless[F[_[_], _[_], _], R1[_], R2[_]](implicit F: RdfTagless[[X] => F[R1, R2, X]]) = new TaglessAux2[F, R1, R2] {
    override def apply: RdfTagless[R] = F
  }
}

//trait RdfTagless_LowLowPriority {
//  implicit def fromAux2[F[_[_], _[_], _], R1[_], R2[_]](implicit aux2: TaglessAux2[F, R1, R2]) = aux2.apply
//}

trait RdfTagless_LowPriority {

  implicit def prodTagless[R1[_], R2[_]](implicit t1: RdfTagless[R1], t2: RdfTagless[R2])
  : RdfTagless[[X] => Prod[R1, R2, X]] = new RdfTagless[[X] => Prod[R1, R2, X]] {
=======
trait RdfTagless_LowPriority {

  implicit def prodTagless[R1[_], R2[_]](implicit t1: RdfTagless[R1], t2: RdfTagless[R2])
  : RdfTagless[Prod[R1, R2, ?]] = new RdfTagless[Prod[R1, R2, ?]] {
>>>>>>> a5723e10ab7e181fb661f297a5681dc5aa59e796

    override def iri(toString: String): Prod[R1, R2, Iri] =
      Prod(t1.iri(toString), t2.iri(toString))

    override def bNode(localName: String): Prod[R1, R2, BNode] =
      Prod(t1.bNode(localName), t2.bNode(localName))

    override def literal(lexicalValue: String,
                         languageCode: Option[String],
                         xsdType: Option[String]): Prod[R1, R2, Literal] =
      Prod(t1.literal(lexicalValue, languageCode, xsdType), t2.literal(lexicalValue, languageCode, xsdType))

    override def iriAsSubject(iri: Prod[R1, R2, Iri]): Prod[R1, R2, Subject] =
      Prod(t1.iriAsSubject(iri.first), t2.iriAsSubject(iri.second))

    override def bNodeAsSubject(bNode: Prod[R1, R2, BNode]): Prod[R1, R2, Subject] =
      Prod(t1.bNodeAsSubject(bNode.first), t2.bNodeAsSubject(bNode.second))

    override def iriAsPredicate(iri: Prod[R1, R2, Iri]): Prod[R1, R2, Predicate] =
      Prod(t1.iriAsPredicate(iri.first), t2.iriAsPredicate(iri.second))

    override def iriAsObject(iri: Prod[R1, R2, Iri]): Prod[R1, R2, Object] =
      Prod(t1.iriAsObject(iri.first),  t2.iriAsObject(iri.second))

    override def bNodeAsObject(bNode: Prod[R1, R2, BNode]): Prod[R1, R2, Object] =
      Prod(t1.bNodeAsObject(bNode.first),  t2.bNodeAsObject(bNode.second))

    override def literalAsObject(literal: Prod[R1, R2, Literal]): Prod[R1, R2, Object] =
      Prod(t1.literalAsObject(literal.first),  t2.literalAsObject(literal.second))

    override def iriAsGraphName(iri: Prod[R1, R2, Iri]): Prod[R1, R2, GraphName] =
      Prod(t1.iriAsGraphName(iri.first),  t2.iriAsGraphName(iri.second))

    override def bNodeAsGraphName(bNode: Prod[R1, R2, BNode]): Prod[R1, R2, GraphName] =
      Prod(t1.bNodeAsGraphName(bNode.first),  t2.bNodeAsGraphName(bNode.second))

    override def triple(subject: Prod[R1, R2, Subject],
                        predicate: Prod[R1, R2, Predicate],
                        `object`: Prod[R1, R2, Object]): Prod[R1, R2, Triple] =
      Prod(t1.triple(subject.first, predicate.first, `object`.first),  t2.triple(subject.second, predicate.second, `object`.second))

    override def quad(subject: Prod[R1, R2, Subject],
                      predicate: Prod[R1, R2, Predicate],
                      `object`: Prod[R1, R2, Object],
                      graph: Prod[R1, R2, GraphName]): Prod[R1, R2, Quad] =
      Prod(t1.quad(subject.first, predicate.first, `object`.first, graph.first),  t2.quad(subject.second, predicate.second, `object`.second, graph.second))

    override def graph(triples: Prod[R1, R2, Triple]*): Prod[R1, R2, Graph] =
      Prod(t1.graph(triples.map(_.first) :_*),  t2.graph(triples.map(_.second) :_*))

    override def dataset(defaultGraph: Prod[R1, R2, Graph],
                         namedGraphs: Map[Prod[R1, R2, GraphName], Prod[R1, R2, Graph]]): Prod[R1, R2, Dataset] = ???
//      Prod(t1.dataset(defaultGraph.first, namedGraphs map { case (k, v) => k.first,  v.first }),  t2.dataset(defaultGraph.second, namedGraphs map { case (k, v) => k.second,  v.second }))
  }

//  implicit def bothProperties[R1[_], R2[_]](Prod(t1: RdfProperties[R1], t2: RdfProperties[R2]))
//    : RdfProperties[BothAux[R1, R2]#of] = new RdfProperties[BothAux[R1, R2]#of] {
//
//    override def subjectOfTriple(triple: Prod[R1, R2, Triple]): Prod[R1, R2, Subject] =
//      Prod(t1.subjectOfTriple(triple.first),  t2.subjectOfTriple(triple.second))
//
//    override def predicateOfTriple(triple: Prod[R1, R2, Triple]): Prod[R1, R2, Predicate] =
//      Prod(t1.predicateOfTriple(triple.first),  t2.predicateOfTriple(triple.second))
//
//    override def objectOfTriple(triple: Prod[R1, R2, Triple]): Prod[R1, R2, Object] =
//      Prod(t1.objectOfTriple(triple.first),  t2.objectOfTriple(triple.second))
//
//    override def subjectOfQuad(quad: Prod[R1, R2, Quad]): Prod[R1, R2, Subject] =
//      Prod(t1.subjectOfQuad(quad.first),  t2.subjectOfQuad(quad.second))
//
//    override def predicateOfQuad(quad: Prod[R1, R2, Quad]): Prod[R1, R2, Predicate] =
//      Prod(t1.predicateOfQuad(quad.first),  t2.predicateOfQuad(quad.second))
//
//    override def objectOfQuad(quad: Prod[R1, R2, Quad]): Prod[R1, R2, Object] =
//      Prod(t1.objectOfQuad(quad.first),  t2.objectOfQuad(quad.second))
//
//    override def graphOfQuad(quad: Prod[R1, R2, Quad]): Prod[R1, R2, GraphName] =
//      Prod(t1.graphOfQuad(quad.first),  t2.graphOfQuad(quad.second))
//
//  }

}
