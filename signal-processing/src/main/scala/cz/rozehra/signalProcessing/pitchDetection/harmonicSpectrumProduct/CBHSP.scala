package cz.rozehra.signalProcessing.pitchDetection.harmonicSpectrumProduct

import cz.rozehra.signalProcessing.pitchDetection.FundamentalsDetection
import cz.rozehra.signalProcessing._
import scala.math._

object CBHSP extends FundamentalsDetection {
  val harmonicsCount = 5

  override def detectFundamentals(signal: TimeDomainWaveForm[Signal]):
    List[Seq[(Frequency, Double)]] = {

    val windows = signal.segmentToWindows(4096, 2048).windows.
      map(w => w.hanningWindow.changeMaxFrameSize(8192))

    windows.map(processWindow)
  }

  private def processWindow(window: Window[Signal]) = {
    val spectrum =  window.toSpectrum
    val hsp = HSP.computeHSP(spectrum, harmonicsCount)
    val fic = frequencyIndexedCepstrum(window, hsp.size)

    val cbhsp = for (i <- 0 until hsp.size) yield hsp(i) * fic(i)

    val startBin = 100 / spectrum.maxFrequency * spectrum.bandsCount
    val endBin = 600 / spectrum.maxFrequency * spectrum.bandsCount

    val maxIndex = cbhsp.zipWithIndex.filter( x => x._2 > startBin && x._2 < endBin).maxBy(_._1)._2
    val maxFrequency = maxIndex * spectrum.bandWidth
    Seq((maxFrequency, cbhsp(maxIndex)))
  }


  private def frequencyIndexedCepstrum(window: Window[Signal], take: Int) = {
    val transformed = new FreqDomainWindow(window)
    val realCepstrum = fft.FFT.fft(transformed.values.map(c => log(pow(c.abs, 2)))).map(c => pow(c.abs, 2))

    val frequencyIndexedCepstrum = collection.mutable.IndexedSeq.fill(realCepstrum.size)(0.0)
    for (i <- 0 until realCepstrum.size) {
      val indexToInsert = window.size / (i + 1)
      frequencyIndexedCepstrum(indexToInsert) = max(frequencyIndexedCepstrum(indexToInsert), realCepstrum(i))
    }
    frequencyIndexedCepstrum.take(take)
  }

  def spectrogramSamplingRate(signalSamplingRate: Double): Double = signalSamplingRate / 2048
}
