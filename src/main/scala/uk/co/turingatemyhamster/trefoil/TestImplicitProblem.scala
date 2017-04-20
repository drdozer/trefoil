package uk.co.turingatemyhamster.trefoil

import cats.Id
import cats.data.{Const, Prod}

/**
  *
  *
  * @author Matthew Pocock
  */
object TestImplicitProblem {
  //    def main(args: Array[String]): Unit = {

//  trait MyTypeclass[R[_]]
//
//  implicit object StringImpl extends MyTypeclass[Const[String, ?]]
//  implicit object IdImpl extends MyTypeclass[Id]
//
//  implicit def productImpl[R1[_], R2[_]](implicit R1: MyTypeclass[R1], R2: MyTypeclass[R2])
//  : MyTypeclass[Prod[R1, R2, ?]] = new MyTypeclass[Prod[R1, R2, ?]] {}

//  trait App1[F[_, _], A] {
//    type Out[T] = F[A, T]
//    def apply: MyTypeclass[Out]
//  }
//
//  implicit def app1[F[_, _], A](implicit MT: MyTypeclass[F[A, ?]]) = new App1[F, A] {
//    def apply: MyTypeclass[Out] = MT
//  }
//
//  implicit def fromApp1[F[_, _], A](implicit A: App1[F, A]) = A.apply
//
//  trait App2[F[_[_], _[_], _], R1[_], R2[_]] {
//    type Out[T] = F[R1, R2, T]
//    def apply: MyTypeclass[Out]
//  }
//
//  implicit def app2[F[_[_], _[_], _], R1[_], R2[_]](implicit MT: MyTypeclass[F[R1, R2, ?]]) = new App2[F, R1, R2] {
//    override def apply: MyTypeclass[Out] = MT
//  }
//
//  implicit def fromApp2[F[_[_], _[_], _], R1[_], R2[_]](implicit A: App2[F, R1, R2]) = A.apply

  // finds  this
//  type si[T] = Const[String, T]
//  val psiTpe = implicitly[MyTypeclass[si]]

  // finds both of these
//  val si = implicitly[MyTypeclass[Const[String, ?]]]
//  val idI = implicitly[MyTypeclass[Id]]

  // can't find this
//  val psi = implicitly[MyTypeclass[Prod[Id, Const[String, ?], ?]]]

  // can't find this
//  type psiT[T] = Prod[Id, Const[String, ?], T]
//  val psiT = implicitly[MyTypeclass[psiT]]

  // nor this
//  val psiPTpe = implicitly[MyTypeclass[Prod[Id, si, ?]]]

  // but can find this
//  type psiTTpe[T] = Prod[Id, si, T]
//  val psiTTpe = implicitly[MyTypeclass[psiTTpe]]
  //    }
}
