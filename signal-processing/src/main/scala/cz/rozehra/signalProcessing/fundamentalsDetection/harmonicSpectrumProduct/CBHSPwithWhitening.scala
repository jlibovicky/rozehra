package cz.rozehra.signalProcessing.fundamentalsDetection.harmonicSpectrumProduct

import cz.rozehra.signalProcessing.fundamentalsDetection.FundamentalsDetection
import cz.rozehra.signalProcessing._
import fundamentalsDetection.klapuriWhitening.Whitening
import math._

object CBHSPwithWhitening extends FundamentalsDetection {
  val harmonicsCount = 5

  def detectFundamentals(signal: TimeDomainWaveForm[Signal]): List[Seq[(Frequency, Double)]] = {
    val whitenedFreqDomain = Whitening.whitenedFreqDomain(signal)
    whitenedFreqDomain.map(processFreqWindow)
  }

  def spectrogramSamplingRate(signalSamplingRate: Double): Double = signalSamplingRate / 2048

  private def processFreqWindow(freqWindow: FreqDomainWindow) = {
    val spectrum =  freqWindow.toSpectrum
    val hsp = HSP.computeHSP(spectrum, harmonicsCount)
    val fic = frequencyIndexedCepstrum(freqWindow, hsp.size)

    val cbhsp = for (i <- 0 until hsp.size) yield hsp(i) * fic(i)

    val startBin = 100 / spectrum.maxFrequency * spectrum.bandsCount
    val endBin = 600 / spectrum.maxFrequency * spectrum.bandsCount

    val maxIndex = cbhsp.zipWithIndex.filter( x => x._2 > startBin && x._2 < endBin).maxBy(_._1)._2
    val maxFrequency = maxIndex * spectrum.bandWidth
    Seq((maxFrequency, cbhsp(maxIndex)))
  }


  private def frequencyIndexedCepstrum(freqWindow: FreqDomainWindow, take: Int) = {
    val realCepstrum = fft.FFT.fft(freqWindow.values.map(c => log(pow(c.abs, 2)))).map(c => pow(c.abs, 2))

    val frequencyIndexedCepstrum = collection.mutable.IndexedSeq.fill(realCepstrum.size)(0.0)
    for (i <- 0 until realCepstrum.size) {
      val indexToInsert = freqWindow.length / (i + 1) - 1
      frequencyIndexedCepstrum(indexToInsert) = max(indexToInsert, realCepstrum(i))
    }
    frequencyIndexedCepstrum.take(take)
  }
}
