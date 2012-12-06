package cz.rozehra.signalProcessing

import fft.FFT
import scala.math._
import scalala.tensor.dense._
import scalala.library.Plotting

class Spectrum[T <: Double](val withWindowShift: Int, val bandWidth: Frequency, val amplitudes: IndexedSeq[T]) {
  def maxFrequency = bandWidth * amplitudes.length
  def magnitute[T : ClassManifest]: String = classManifest[T].erasure.getName

  val samplingRate = 2 * maxFrequency
  val duration: Time = withWindowShift / samplingRate

    def plot: Unit = {
      Plotting.plot(DenseVector.range(0, amplitudes.length) * bandWidth, new DenseVectorRow(amplitudes.toArray[Double]))
      Plotting.title("A spectrum of " + magnitute)
      Plotting.xlabel("Frequency in Hz")
      Plotting.ylabel(magnitute)
    }
    
    def plot(fileName: String): Unit = {
      plot
      Plotting.saveas(fileName)
    }
}