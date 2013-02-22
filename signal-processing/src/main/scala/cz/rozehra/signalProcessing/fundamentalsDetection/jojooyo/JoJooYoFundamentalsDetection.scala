package cz.rozehra.signalProcessing.fundamentalsDetection.jojooyo

import cz.rozehra.signalProcessing.fundamentalsDetection.FundamentalsDetection
import cz.rozehra.signalProcessing.Spectrogram
import cz.rozehra.signalProcessing.Signal
import math._

object JoJooYoFundamentalsDetection extends FundamentalsDetection {
  val usedHarmonics = 10
  val minFrequencyCents = round(f_cent(20)).toInt
  val maxFrequencyCents = round(f_cent(4200)).toInt
  val spectrumBound = round(f_cent(20050)).toInt
  val nBestSize = 3
  val variance = 1.0

  def f_cent(frequency: Double) = 6900.0 + 1200 * log(frequency / 440.0) / log(2)
  def centsToFrequency(cents: Double) = 440.0 * pow(2, (cents - 6900) / 1200.0)

  def detectFundamentals(spectrogram: Spectrogram[Signal], originalSamplingRate: Double) = {
    spectrogram.spectra.map(new JoJooYoFundamentalsDetector(_).fundamentals).toList
  }


  private def G(x: Double, x0: Double, sigma: Double) =
    1 / sqrt(2 * Pi * sigma * sigma) * exp(- pow(x - x0, 2) / 2 / sigma / sigma)

  def GFunction(k: Int, omega: Int, m: Int) = G(k, omega + 1200.0 * log(m) / log(2), variance)
}
