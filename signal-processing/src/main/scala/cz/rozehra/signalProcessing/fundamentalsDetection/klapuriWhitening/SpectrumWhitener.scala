package cz.rozehra.signalProcessing.fundamentalsDetection.klapuriWhitening

import scala.math._
import cz.rozehra.signalProcessing._
import org.apache.commons.math3.complex.Complex

class SpectrumWhitener(val transform: FreqDomainWindow) {
  val nu = 0.33
  val c = (1 to 30).map( (b) => 229 * (pow(10, (b + 1) / 21.4) - 1)).toIndexedSeq
  val bandWidth = transform.samplingRate / 2 / transform.length

  private def H(b: Int, k: Int) = {
    val x = bandWidth * k  + bandWidth / 2
    val bIndex = b - 1 // 1-based b transformed to 0 index seq c

    if (bIndex > 0 && bIndex < 30 && x > c(bIndex - 1) && x <= c(bIndex))
      x / (c(bIndex) - c(bIndex - 1)) - c(bIndex - 1) / (c(bIndex) - c(bIndex - 1))
    else if (bIndex > 0 && bIndex < 29 && x > c(bIndex) && x < c(bIndex + 1))
      x / (c(bIndex) - c(bIndex + 1)) + c(bIndex + 1) / (c(bIndex + 1) - c(bIndex))
    else 0.0
  }

  val sigma = (1 to 30).map( (b) => sqrt( 1.0 / transform.length *
    (0 until transform.length).foldLeft(0.0)( (sum, k) =>
      sum + (H(b, k) * transform.values(k).abs * transform.values(k).abs))))

  val gammaB = (0 until 30).map( (b) => pow(sigma(b), nu)).toIndexedSeq

  private def gamma(k: Int) = {
    val f_k = bandWidth * k + bandWidth / 2

    val b_k = min(max(1.0, 21.4 * log10(f_k / 229 + 1) - 1), 30.0)
    val b_1 = if (b_k > 0.0) floor(b_k).toInt else 0
    val b_2 = if (b_k > 0.0) ceil(b_k).toInt else 0

    if (b_1 == b_2) gammaB(b_1 - 1)
    else gammaB(b_1 - 1) + (b_k - b_1) / (b_2 - b_1) * (gammaB(b_2 - 1)  - gammaB(b_1 - 1))
  }

  def getWhitenedSpectrum = {
    var newAmplitudesRev = List.empty[Complex]
    for (k <- 0 until transform.length)
      newAmplitudesRev ::= transform.values(k) multiply gamma(k)

    val spectrum = newAmplitudesRev.reverse.toIndexedSeq.take((transform.length / 2)).map(n => n.abs)
    new Spectrum[Signal](2 * transform.withWindowShift, transform.samplingRate / spectrum.length, spectrum)
  }

  def getWhitenedFreqDomainWindow = {
    var newAmplitudesRev = List.empty[Complex]
    for (k <- 0 until transform.length)
      newAmplitudesRev ::= transform.values(k) multiply gamma(k)
    new FreqDomainWindow(transform.withWindowShift, transform.samplingRate, newAmplitudesRev.reverse.toIndexedSeq)
  }
}
