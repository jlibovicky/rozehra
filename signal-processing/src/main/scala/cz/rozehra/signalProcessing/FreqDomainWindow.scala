package cz.rozehra.signalProcessing

import org.apache.commons.math3.complex.Complex

class FreqDomainWindow(val withWindowShift: Int, val samplingRate: Frequency, val values: IndexedSeq[Complex]) {
  def this(window: Window[Signal]) = this(window.withShift, window.samplingRate,
             fft.FFT.fft(fft.FFT.hanningWindow(window.samples) ++ IndexedSeq.fill(window.maxFrameSize - window.samples.size)(0.0)))

  def length = values.size

  def toSpectrum = new Spectrum[Signal](withWindowShift, samplingRate / length / 2, values.take(length / 2).map(_.abs))
}
