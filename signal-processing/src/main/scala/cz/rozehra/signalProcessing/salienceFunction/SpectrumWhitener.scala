package cz.rozehra.signalProcessing.salienceFunction

import scala.math._
import cz.rozehra.signalProcessing._

class SpectrumWhitener(val spectrum: Spectrum[Signal]) {
  val nu = 0.33
  val c = (1 to 30).map( (b) => 229 * (pow(10, (b + 1) / 21.4) - 1))

  private def H(b: Int, k: Int) = {
    val x = spectrum.bandWidth * k  + spectrum.bandWidth / 2
    val bIndex = b - 1 // 1-based b transformed to 0 index seq c

    if (bIndex > 0 && bIndex < 30 && x > c(bIndex - 1) && x <= c(bIndex))
      x / (c(bIndex) - c(bIndex - 1)) - c(bIndex - 1) / (c(bIndex) - c(bIndex - 1))
    else if (bIndex > 0 && bIndex < 29 && x > c(bIndex) && x < c(bIndex + 1))
      x / (c(bIndex) - c(bIndex + 1)) + c(bIndex + 1) / (c(bIndex + 1) - c(bIndex))
    else 0.0
  }

  val sigma = (1 to 30).map( (b) => sqrt( 1.0 / spectrum.amplitudes.size *
    (0 until spectrum.amplitudes.size).foldLeft(0.0)( (sum, k) =>
      sum + H(b, k) * spectrum.amplitudes(k) * spectrum.amplitudes(k))))

  val gammaB = (0 until 30).map( (b) => pow(sigma(b), nu))

  private def gamma(k: Int) = {
    val f_k = spectrum.bandWidth * k + spectrum.bandWidth / 2

    val b_k = min(max(1.0, 21.4 * log10(f_k / 229 + 1) - 1), 30.0)
    val b_1 = if (b_k > 0.0) floor(b_k).asInstanceOf[Int] else 0
    val b_2 = if (b_k > 0.0) ceil(b_k).asInstanceOf[Int] else 0

    if (b_1 == b_2) gammaB(b_1 -1 )
    else gammaB(b_1 - 1) + (b_k - b_1) / (b_2 - b_1) * (gammaB(b_2 - 1)  - gammaB(b_1 - 1))
  }

  def getWhitenedSpectrum = {
    var newAmplitudes = IndexedSeq.empty[Signal]
    for (k <- 0 until spectrum.amplitudes.size)
      newAmplitudes = newAmplitudes :+ gamma(k) * spectrum.amplitudes(k)

    new Spectrum[Signal](spectrum.withWindowShift, spectrum.bandWidth, newAmplitudes)
  }
}
