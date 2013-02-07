package cz.rozehra.signalProcessing.fft

import scala.math._
import cz.rozehra.signalProcessing.Window
import collection.{mutable, immutable}
import org.apache.commons.math3.transform.{DftNormalization, FastFourierTransformer, TransformType}
import org.apache.commons.math3.complex.Complex

object FFT {
  private val transformer = new FastFourierTransformer(DftNormalization.STANDARD)

  def powerSpectrum[T <: Double](input: IndexedSeq[T]) = {
    val data = padder(input)
    val outComplex = fft(data)
    val out = outComplex.take((data.length / 2) + 1).map(_.abs).toIndexedSeq
    out.map(i => (i / sqrt(data.length)).asInstanceOf[T]) // Power Spectral Density Output
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

  private def padder(data: Seq[Double]) : Seq[Double] = {
    val closestPow2 = pow(2, ceil(log(data.size) / log(2.0))).asInstanceOf[Int]

    if (data.size == closestPow2) data
    else data ++ Seq.fill(closestPow2 - data.size)(0.0)
  } 

  def fft(f: Seq[Double]) : Seq[Complex] = {
    require(isPowerOf2(f.size), "Input of FFT must have size of power of two.")
    transformer.transform(f.toArray, TransformType.FORWARD).toSeq
  }

  private def isPowerOf2(n: Int): Boolean =
   if (n == 2 || n == 1) true
   else if (n % 2 == 1) false
        else isPowerOf2(n / 2)
}