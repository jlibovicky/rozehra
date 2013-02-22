package cz.rozehra.signalProcessing.fundamentalsDetection.jojooyo

import cz.rozehra.signalProcessing.{Filters, Signal, Frequency, Spectrum}
import scala.math._

class JoJooYoFundamentalsDetector(val spectrum: Spectrum[Signal]) {
  def f_cent(frequency: Double) = JoJooYoFundamentalsDetection.f_cent(frequency)
  def centsToFrequency(cents: Double) = JoJooYoFundamentalsDetection.centsToFrequency(cents)
  private val minFrequencyCents = JoJooYoFundamentalsDetection.minFrequencyCents
  private val maxFrequencyCents = JoJooYoFundamentalsDetection.maxFrequencyCents
  private val spectrumMaxCents = f_cent(spectrum.maxFrequency).toInt
  private val usedHarmonics = JoJooYoFundamentalsDetection.usedHarmonics
  private val nBestSize = JoJooYoFundamentalsDetection.nBestSize
  private val step = 100

  def H(omega: Int, k: Int) =
    (1 to usedHarmonics).foldLeft(0.0)((sum, m) => sum +
      spectrum.amplitudeAtFrequency(centsToFrequency(omega + 1200 * log(m) / log(2)))
        * JoJooYoFundamentalsDetection.GFunction(k, omega, m))

  def J(omega: Int) =
    (0 until spectrumMaxCents by step).foldLeft(0.0)((sum, k) => sum +
      spectrum.amplitudeAtFrequency(omega) * H(omega, k))

  val JValues = (minFrequencyCents to maxFrequencyCents by step).map(J(_))

  val fundamentals: Seq[(Frequency, Double)] = {
    val peaks = Filters.findPeaks(0.15, JValues)
    if (peaks.size <= nBestSize) peaks.map(i => (centsToFrequency(minFrequencyCents + step * i._1), i._2)).toSeq
    else {
      var rest = peaks
      var nBest = Seq.empty[(Frequency, Double)]
      for (i <- 0 to nBestSize) {
        val maxFund = rest.maxBy(_._2)
        nBest :+= ((centsToFrequency(minFrequencyCents + step * maxFund._1), maxFund._2))
        rest -= maxFund
      }
      nBest
    }
  }
}
