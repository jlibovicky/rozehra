package cz.rozehra.signalProcessing.pitchDetection

import cz.rozehra.signalProcessing._
import harmonicSpectrumProduct.CBHSP
import klapuriWhitening.KlapuriFundamentalDetection
import scala.math._


object CombinedFundamentalsDetection extends FundamentalsDetection {
  def detectFundamentals(signal: TimeDomainWaveForm[Signal]): List[Seq[(Frequency, Double)]] = {
    val cbhspResult = CBHSP.detectFundamentals(signal)
    val klapuriResult = KlapuriFundamentalDetection.detectFundamentals(signal)

    def mergeLists(l1: List[Seq[(Frequency, Double)]], l2: List[Seq[(Frequency, Double)]],
                   acc: List[Seq[(Frequency, Double)]]): List[Seq[(Frequency, Double)]] = {
      l1 match {
        case Nil => acc.reverse
        case l1head :: l1tail => {
          val l2head = l2.head
          val l2tail = l2.tail

          val mergedSeqs = (l2head ++ l1head).sortBy(_._1)
          var freqs = if (mergedSeqs.nonEmpty) List(mergedSeqs.head._1) else List.empty
          if (mergedSeqs.nonEmpty)
            for ( (f, v) <- mergedSeqs.drop(1)) {
              if (abs(freqToTone(f) - freqToTone(freqs.head)) > 0.25) freqs ::= f
            }

          val nextSeq = freqs.map((_, 1.0)).toSeq
          mergeLists(l1tail, l2tail, nextSeq :: acc)
        }
      }
    }

    mergeLists(cbhspResult, klapuriResult, List.empty)

  }

  def spectrogramSamplingRate(signalSamplingRate: Double): Double = signalSamplingRate / 2048

  private def freqToTone(f: Double) =  69 + 12 * log(f / 440.0) / log(2)
}
