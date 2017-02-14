package uk.co.turingatemyhamster.trefoil

import cats._


trait StoreFactory[Store] {
  def create: Store
}

object StoreFactory {
  implicit object denseByteLongIndexStoreFactory extends StoreFactory[Array[Long]] {
    override def create: Array[Long] =
      Array.ofDim[Long](ByteOps[Int].lowestByteHot)
  }
}


/**
  *
  *
  * @author Matthew Pocock
  */
trait IndexedStore[Store, K, V, F[_]] {
  def get(impl: Store, k: K): F[V]
}

object IndexedStore {
  implicit object denseByteLongIndexedStore extends IndexedStore[Array[Long], Byte, Long, Id] {
    override def get(array: Array[Long], k: Byte): Long =
      array(k.toInt)
  }

  implicit def denseByteIndexedStore[T]: IndexedStore[Array[T], Byte, T, Id] = new IndexedStore[Array[T], Byte, T, Id]
  {
    override def get(array: Array[T], k: Byte): T =
      array(k.toInt)
  }

  def mapKey[Store, K1, K2, V, F[_]](is1: IndexedStore[Store, K1, V, F], fk: K2 => K1)
  : IndexedStore[Store, K2, V, F] = new IndexedStore[Store, K2, V, F]
  {
    override def get(impl: Store, k: K2): F[V] =
      is1.get(impl, fk(k))
  }

  def mapValue[Store, K, V1, V2, F[_]](is1: IndexedStore[Store, K, V1, F], fv: V1 => V2)(implicit F: Functor[F])
  : IndexedStore[Store, K, V2, F] = new IndexedStore[Store, K, V2, F] {
    override def get(impl: Store, k: K): F[V2] =
      F.map(is1.get(impl, k))(fv)
  }

  def mapContainer[Store, K, V, F1[_], F2[_]](is1: IndexedStore[Store, K, V, F1], ff: F1 ~> F2)
  : IndexedStore[Store, K, V, F2] = new IndexedStore[Store, K, V, F2] {
    override def get(impl: Store, k: K): F2[V] =
      ff(is1.get(impl, k))
  }
}
