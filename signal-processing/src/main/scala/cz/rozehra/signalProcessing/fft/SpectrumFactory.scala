package cz.rozehra.signalProcessing.fft

import cz.rozehra.signalProcessing._

object SpectrumFactory {
  def fillWithZerosToPowerOf2[T](original: Window[T]): Window[T] = null
  
  def hammingWindow[T <: Double](original: Window[T]) = {
    def hammingWindow0(original: List[T], n: Int, N: Int, accu: List[T]): List[T] =
      original match {
        case Nil => accu.reverse
        case x :: xs => hammingWindow0(xs, n + 1, N, 
            (x * 0.5 * (1.0 - math.cos(2.0 * math.Pi * n / (N - 1.0)))).asInstanceOf[T] :: accu)          
    }
      
    new Window[T](original.samplingRate, hammingWindow0(original.samples, 0, original.size, Nil))
  }
  
  def castToT[T <: Double](numbers: List[Double]): List[T] = null
 
  def computeSpectrum[T <: Double](signal: Window[T]): Spectrum[T] = {
    val duration: Time = signal.size / signal.samplingRate    
    val spectrum: List[T] = castToT[T](FFT.powerSpectrum(signal.samples.toIndexedSeq).toList)
    val bandWidth: Frequency = signal.samplingRate / 2 / spectrum.length 
    new Spectrum(duration, bandWidth, spectrum)
  }
}