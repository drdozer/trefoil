package uk.co.turingatemyhamster.trefoil

trait Bytes[T] {
  def sizeInBytes: Int
  def sizeInBits: Int = sizeInBytes * 8
}

object Bytes {

  def apply[T](implicit b: Bytes[T]): Bytes[T] = b

  implicit object byteInBytes extends Bytes[Byte] {
    def sizeInBytes = 1
  }

  implicit object shortInBytes extends Bytes[Short] {
    def sizeInBytes = 2
  }

  implicit object charInBytes extends Bytes[Char] {
    def sizeInBytes = 2
  }

  implicit object intInBytes extends Bytes[Int] {
    def sizeInBytes = 4
  }

  implicit object longInBytes extends Bytes[Long] {
    def sizeInBytes = 8
  }

  implicit object doubleInBytes extends Bytes[Double] {
    def sizeInBytes = 8
  }

  implicit def pairInBytes[A : Bytes, B : Bytes]: Bytes[(A, B)] = new Bytes[(A, B)] {
    override def sizeInBytes: Int = Bytes[A].sizeInBytes + Bytes[B].sizeInBytes
  }

  implicit def tuple3InBytes[A : Bytes, B : Bytes, C: Bytes]: Bytes[(A, B, C)] = new Bytes[(A, B, C)] {
    override def sizeInBytes: Int = Bytes[A].sizeInBytes + Bytes[B].sizeInBytes + Bytes[C].sizeInBytes
  }

  implicit def tuple4InBytes[A : Bytes, B : Bytes, C : Bytes, D : Bytes]: Bytes[(A, B, C, D)] = new Bytes[(A, B, C, D)] {
    override def sizeInBytes: Int = Bytes[A].sizeInBytes + Bytes[B].sizeInBytes + Bytes[C].sizeInBytes + Bytes[D].sizeInBytes
  }
}

trait ValueBytes[T] {
  def bytes(t: T): Bytes[T]
}

object ValueBytes {
  def apply[T](implicit vb: ValueBytes[T]): ValueBytes[T] = vb

  implicit def forBytes[T : Bytes]: ValueBytes[T] = new ValueBytes[T] {
    override def bytes(t: T): Bytes[T] = Bytes[T]
  }

  // Arrays with heterogeneously sized elemenets
  implicit def forArrayByElements[T](implicit T: ValueBytes[T]): ValueBytes[Array[T]] = new ValueBytes[Array[T]] {
    override def bytes(t: Array[T]): Bytes[Array[T]] = new Bytes[Array[T]] {
      override def sizeInBytes: Int =
        t.map(T.bytes(_).sizeInBytes).sum
    }
  }

  // Arrays with homogeneously sized elemeents
  implicit def forArray[T](implicit T: Bytes[T]): ValueBytes[Array[T]] = new ValueBytes[Array[T]] {
    override def bytes(t: Array[T]): Bytes[Array[T]] = new Bytes[Array[T]] {
      override def sizeInBytes: Int = t.length * T.sizeInBytes
    }
  }
}

trait ByteOps[T] {
  def lowestByteHot: T
  def lowestByte(t: T): Byte
  def shiftByte(t: T): T
}

object ByteOps {

  def apply[T](implicit bo: ByteOps[T]): ByteOps[T] = bo

  implicit object intByteOps extends ByteOps[Int] {
    override def lowestByteHot: Int = 255

    override def lowestByte(t: Int): Byte = (t & lowestByteHot).toByte

    override def shiftByte(t: Int): Int = t << Bytes[Byte].sizeInBits
  }

  implicit object longByteOps extends ByteOps[Long] {
    override def lowestByteHot = 255l

    override def lowestByte(t: Long): Byte = (t & lowestByteHot).toByte

    override def shiftByte(t: Long): Long = t << Bytes[Byte].sizeInBits
  }


}



