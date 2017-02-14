package uk.co.turingatemyhamster.trefoil

sealed trait ∅


/** Every instance of `Tpe` can be represented as an instance consistent with `SubTpeExp`. */
trait ⊑ [Tpe, SubTpeExp]

/** Every instance of this type union can be represented as either `A` or `B`. */
trait ⊔ [A, B]

/** Every instance of this type sum can be represented as an instance of A and of B. */
trait ⊓ [A, B]


trait Core {

  type Iri
  type BNode
  type Literal

  type Graph
  type Subject
  type Predicate
  type Object
  type Triple
  type Quad

}

trait Types[C <: Core] {
  protected val core: C
  import core._

  implicit def subjectIsIriOrBNode: Subject ⊑ (Iri ⊔ BNode ⊔ ∅)

  implicit def predicateIsIri: Predicate ⊑ Iri

  implicit def objectIsIriOrBNodeOrLiteral: Object ⊑ (Iri ⊔ BNode ⊔ Literal ⊔ ∅)

  implicit def tripleIsSubjectAndPredicateAndObject: Triple ⊑ (Subject ⊓ Predicate ⊓ Object)

  implicit def quadIsSubjectAndPredicateAndObjectAndGraph: Quad ⊑ (Subject ⊓ Predicate ⊓ Object ⊓ Graph)
}


