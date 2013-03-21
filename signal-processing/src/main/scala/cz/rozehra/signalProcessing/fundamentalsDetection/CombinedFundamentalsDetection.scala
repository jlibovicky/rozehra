package cz.rozehra.signalProcessing.fundamentalsDetection

import cz.rozehra.signalProcessing._
import harmonicSpectrumProduct.CBHSP
import klapuriWhitening.KlapuriFundamentalDetection
import scala.math._


object CombinedFundamentalsDetection extends FundamentalsDetection {
  def detectFundamentals(signal: TimeDomainWaveForm[Signal]): List[Seq[(Frequency, Double)]] = {
    val cbhspResult = CBHSP.detectFundamentals(signal)
    val klapuriResult = KlapuriFundamentalDetection.detectFundamentals(signal)

    var finalListRev = List.empty[Seq[(Frequency, Double)]]
    for ( (cbhsp, klap) <- cbhspResult zip klapuriResult ) {
      val pitchCandidate = freqToTone(cbhsp.head._1)
      val plausibleCandidates = klap.filter(f => abs(pitchCandidate - freqToTone(f._1)) < 0.25)
      if (plausibleCandidates.isEmpty) finalListRev ::= Seq.empty
      else finalListRev ::= Seq(plausibleCandidates.maxBy(_._2))
    }
    finalListRev.reverse
  }

  def spectrogramSamplingRate(signalSamplingRate: Double): Double = signalSamplingRate / 2048

  private def freqToTone(f: Double) =  69 + 12 * log(f / 440.0) / log(2)
}
