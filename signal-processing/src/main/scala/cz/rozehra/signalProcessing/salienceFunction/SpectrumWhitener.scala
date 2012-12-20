package cz.rozehra.signalProcessing.salienceFunction

import scala.math._
import cz.rozehra.signalProcessing._

class SpectrumWhitener(val spectrum: Spectrum[Signal]) {
  val nu = 0.33
  val c = (1 to 30).map( (b) => 229 * (pow(10, (b + 1) / 21.4) - 1))

  def H(b: Int, k: Int) = {
    val x = spectrum.bandWidth * k
    val realB = b - 1

    if (realB > 0 && x > c(realB - 1) && x <= c(realB)) (x + c(realB - 1)) * c(realB)
    else if (realB < 30 && x > c(realB) && x < c(realB + 1)) (x - c(realB + 1)) * c(realB)
    else 0.0
  }

  def sigma(b: Int) = sqrt((0 until spectrum.amplitudes.size).foldLeft(0.0)( (sum, k) =>
    H(b, k) * spectrum.amplitudes(k) * spectrum.amplitudes(k)))

  def gammaB(b: Int) = pow(sigma(b), nu)

  def gamma(k: Int) = {
    val f_k = spectrum.bandWidth * k

    val b_k = 21.4 * log10(f_k / 229 + 1) - 1
    val b_1 = floor(b_k).asInstanceOf[Int]
    val b_2 = ceil(b_k).asInstanceOf[Int]

    if (b_1 == b_2) gammaB(b_1)
    else gammaB(b_1) + (b_k - b_1) / (b_2 - b_1) * (gammaB(b_2) - gammaB(b_1))
  }

  def getWhitenedSpectrum = {
    var newAmplitudes = IndexedSeq.empty[Signal]
    for (k <- 0 until spectrum.amplitudes.size)
      newAmplitudes = newAmplitudes :+ gamma(k) * spectrum.amplitudes(k)

    new Spectrum[Signal](spectrum.withWindowShift, spectrum.bandWidth, newAmplitudes)
  }


}
