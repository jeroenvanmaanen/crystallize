package org.leialearns.crystallize.util

import java.io.{IOException, Reader}
import java.math.BigInteger

import grizzled.slf4j.Logging

import scala.language.implicitConversions
import Integral.Implicits._
import Ordering.Implicits._

class PrefixFreeIntegral[T](implicit val num: Integral[T]) extends Logging {
  type Chunk = (Bit,BitString)
  private val intDigits: Int = Integer.numberOfLeadingZeros(0)
  private val zero: T = 0
  private val one: T = 1
  private val two: T = 1 + 1
  private val maxInt: T = Int.MaxValue
  private val big: Boolean = (maxInt + one) > maxInt

  def prefixEncode(n: T): String = {
    logger.debug(s"Start prefix encode integral: [$n]")
    if (n < zero) {
      throw new IllegalArgumentException("Value should be non-negative")
    }
    val chunks = prefixEncodeChunks(integral2hasLength(n + one), ONE, Nil)
    logger.trace(s"Chunks: [$chunks}]")
    val parts = chunks.foldRight("" :: Nil)(appendChunk)
    parts.mkString
  }

  def descriptionLength(n: T): Long = {
    logger.debug(s"Start description length integral: [$n]")
    if (n < zero) {
      throw new IllegalArgumentException("Value should be non-negative")
    }
    prefixEncodeChunks(integral2hasLength(n + one), ONE, Nil).foldLeft(0L)(addChunk)
  }

  private def prefixEncodeChunks[A <: BitString](n: BitString, lastChunkFlag: Bit, extra: List[Chunk]): List[Chunk] = {
    logger.trace(s"Prefix encode integral: [$n]")
    if (n.isOne) {
      (lastChunkFlag, n) :: extra
    } else {
      val remainder = n.length
      prefixEncodeChunks[A](remainder, ZERO, (lastChunkFlag, n) :: extra)
    }
  }

  private def addChunk(accumulator: Long, chunk: Chunk): Long = {
    accumulator + 1 + Option(chunk._2).map(_.size).getOrElse(0)
  }

  private def appendChunk(chunk: Chunk, pieces: List[String]): List[String] = {
    val (lastChunkFlag, n) = chunk
    val newList = lastChunkFlag.asChar.toString :: ":1" :: n.binary :: "(" :: n.toString :: ")" :: pieces
    if (n.isOne) {
      newList
    } else {
      "/" :: newList
    }
  }

  def prefixDecode(reader: Reader): T = {
    var value: T = zero
    var isLength = false
    var length = 0L
    do {
      isLength = readBit(reader) == ZERO
      length = value.toLong()
      value = one
      for (_ <- 0L to length - 1L) {
        value = value * two
        if (readBit(reader) == ONE) {
          value = value + one
        }
      }
    } while (isLength)
    value - one
  }

  def readBit(reader: Reader): Bit = {
    val n = reader.read()
    if (n == -1) {
      throw new IOException("End of reader")
    }
    n.asInstanceOf[Char] match {
      case 'O' => ZERO
      case 'I' => ONE
      case _ => readBit(reader)
    }
  }

  /**
    * A string of bits encoded as a positive integer by prefixing it with a '1' bit. So the empty string is encoded as 1,
    * the string '0010' as 10010(2) = 18(10)
    */
  trait BitString {
    def length: BitString
    def size: Int
    def binary: String
    def isOne: Boolean
  }

  case class BigIntBitString(value: BigInt) extends BitString {
    override def length = BigIntBitString(size)
    override def size: Int = value.bitLength - 1
    override def binary: String = value toString 2 replace ('0', 'O') replace ('1', 'I') substring 1
    override def isOne: Boolean = value == BigInt(1)
    override def toString: String = String.valueOf(value)
  }

  case class IntBitString(value: Int) extends BitString {
    override def length = IntBitString(size)
    override def size: Int = intDigits - Integer.numberOfLeadingZeros(value) - 1
    override def binary: String = Integer.toBinaryString(value) replace ('0', 'O') replace ('1', 'I') substring 1
    override def isOne: Boolean = value == 1
    override def toString: String = String.valueOf(value)
  }

  implicit def integral2hasLength(n: T): BitString = {
    n match {
      case b: BigInt => BigIntBitString(b)
      case b: BigInteger => BigIntBitString(b)
      case _ =>
        if (big) {
          BigIntBitString(n.toLong)
        } else {
          IntBitString(n.toInt)
        }
    }
  }

  implicit def int2T(n: Int): T = {
    num.fromInt(n)
  }
}

object PrefixFreeIntegral {
  object Implicits {
    implicit object IntIsPrefixFree extends PrefixFreeIntegral[Int]
    implicit object LongIsPrefixFree extends PrefixFreeIntegral[Long]
    implicit object BigIntIsPrefixFree extends PrefixFreeIntegral[BigInt]
  }
}