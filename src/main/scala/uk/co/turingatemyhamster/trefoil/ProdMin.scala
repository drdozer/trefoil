package uk.co.turingatemyhamster

object ProdMin {
  def main(args: Array[String]): Unit = {
    final case class Prod[F[_], G[_], A](first: F[A], second: G[A])
    final case class Const[A, B](getConst: A)
    type Id[A] = A
    trait Encoding[A]
    trait RdfTagless[Rep[_]]
    implicit def rdfTagless[R[_]]: RdfTagless[R] = ???
    def rainbowKb[R[_]](implicit R: RdfTagless[R]) = ???

    type t[X] = Prod[Id, Encoding, X]
    type c[X] = Const[List[String], X]
    type u[X] = Prod[c, t, X]
    rainbowKb[u]
  }
}
