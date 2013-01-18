package cz.rozehra.signalProcessing

import fft.FFT
import scala.math._

class Spectrum[T <: Double](val withWindowShift: Int, val bandWidth: Frequency, val amplitudes: IndexedSeq[T]) {
  def maxFrequency = bandWidth * amplitudes.length
  def magnitute[T : ClassManifest]: String = classManifest[T].erasure.getName


  val samplingRate = 2 * maxFrequency
  val duration: Time = withWindowShift / samplingRate
  val bandsCount = amplitudes.size

  def findPeaks: Set[(Frequency, Double)] = {
    val medianMultiply: Double = 100.0
    val median = (amplitudes.sortWith( (e1, e2) => e1 < e2))(amplitudes.size / 2)

    // spectrum after filtering out values less than median
    var peaks = Set.empty[(Frequency, Double)]
    val s = amplitudes.map( a => if (a > median * medianMultiply) a else 0.0)

    var inNonZeroArea = false
    var tmpMaxIndex = -1

    for (i <- 0 until s.size - 1) {
      if (s(i) > 0 && !inNonZeroArea) {
        inNonZeroArea = true
        tmpMaxIndex = i
      }
      else if (s(i) == 0.0 && inNonZeroArea) {
        inNonZeroArea = false
        peaks += ((tmpMaxIndex.asInstanceOf[Double] * bandWidth, s(tmpMaxIndex)))
        tmpMaxIndex = -1
      }
      else if (s(i) > 0 && inNonZeroArea) {
        if (s(i) > s(tmpMaxIndex)) tmpMaxIndex = i
      }
    }

    peaks
  }

  def plot: Unit = {
    /*Plotting.plot(DenseVector.range(0, amplitudes.length) * bandWidth, new DenseVectorRow(amplitudes.toArray[Double]))
    Plotting.title("A spectrum of " + magnitute)
    Plotting.xlabel("Frequency in Hz")
    Plotting.ylabel(magnitute)*/
  }

  def plot(fileName: String): Unit = {
    /*plot
    Plotting.saveas(fileName)*/
  }
}