package cz.rozehra.signalProcessing.fft

import scala.math._
import cz.rozehra.signalProcessing.Window
import collection.{mutable, immutable}
import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType}
import org.apache.commons.math3.complex.Complex

object FFT {
  private val transformer = new FastFourierTransformer(DftNormalization.STANDARD)

  def powerSpectrum[T <: Double](input: IndexedSeq[T], frameSize: Int) = {
    val data = input ++ IndexedSeq.fill(frameSize - input.size)(0.0)
    val outComplex = fft(data)
    outComplex.take(data.length / 2).par.map(n => (n.abs / sqrt(data.length)).asInstanceOf[T]).toIndexedSeq
  }

  def hammingWindow[T <: Double](original: IndexedSeq[T]): immutable.IndexedSeq[T] = {
    val N = original.size
    val newWindowVector: mutable.IndexedSeq[T] = mutable.IndexedSeq.fill(N)(0.0.asInstanceOf[T])

    for (i <- 0 until original.size)
      newWindowVector(i) = (0.5 * (1.0 - math.cos(2.0 * math.Pi * i / (N - 1.0))) * original(i)).asInstanceOf[T]

    newWindowVector.toIndexedSeq
  }

  def hanningWindow[T <: Double](original: IndexedSeq[T]): immutable.IndexedSeq[T] = {
    val N = original.size
    val newWindowVector: mutable.IndexedSeq[T] = mutable.IndexedSeq.fill(N)(0.0.asInstanceOf[T])

    for (i <- 0 until original.size)
      newWindowVector(i) = (0.5 * (1 - cos(2 * Pi * i / (N - 1.0))) * original(i)).asInstanceOf[T]

    newWindowVector.toIndexedSeq
  }

  def fft(f: IndexedSeq[Double]) : Array[Complex] = {
    require(isPowerOf2(f.size), "Input of FFT must have size of power of two.")
    transformer.transform(f.toArray, TransformType.FORWARD)
  }

  private def isPowerOf2(n: Int): Boolean =
   if (n == 2 || n == 1) true
   else if (n % 2 == 1) false
        else isPowerOf2(n / 2)
}