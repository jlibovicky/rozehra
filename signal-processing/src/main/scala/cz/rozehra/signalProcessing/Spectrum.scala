package cz.rozehra.signalProcessing

import fft.FFT
import scala.math._

class Spectrum[T <: Double](val withWindowShift: Int, val bandWidth: Frequency, val amplitudes: IndexedSeq[T]) {
  def maxFrequency = bandWidth * amplitudes.length
  def magnitute[T : ClassManifest]: String = classManifest[T].erasure.getName


  val samplingRate = 2 * maxFrequency
  val duration: Time = withWindowShift / samplingRate
  val bandsCount = amplitudes.size

  def findPeaks(delta: Double): Set[(Frequency, Double)] = {
    var peaks = Set.empty[(Frequency, Double)]

    var minimum = (Double.NaN, Double.PositiveInfinity)
    var maximum = (Double.NaN, Double.NegativeInfinity)

    var lookForMax = true
    for (i <- 0 until amplitudes.size) {
      val value = amplitudes(i)
      val frequency = i * bandWidth + bandWidth / 2

      if (value > maximum._2) maximum = (frequency, value)
      if (value < minimum._2) minimum = (frequency, value)

      if (lookForMax && value < maximum._2 - delta) {
        peaks += maximum
        minimum = (frequency, value)
        lookForMax = false
      }
      else if (value > minimum._2 + delta) {
        maximum = (frequency, value)
        lookForMax = true
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