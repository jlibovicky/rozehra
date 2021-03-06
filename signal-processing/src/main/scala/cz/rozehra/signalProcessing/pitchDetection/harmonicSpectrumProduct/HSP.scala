package cz.rozehra.signalProcessing.pitchDetection.harmonicSpectrumProduct

import cz.rozehra.signalProcessing.pitchDetection.FundamentalsDetection
import cz.rozehra.signalProcessing._

object HSP extends FundamentalsDetection {
  val harmonicsCount = 5

  override def detectFundamentals(signal: TimeDomainWaveForm[Signal]): List[Seq[(Frequency, Double)]] = {

    val newSpectra = signal.segmentToWindows(4096, 2048).windows.
      map(w => w.changeMaxFrameSize(8192).toSpectrum)
    val spectrogram = new Spectrogram[Signal](1 / newSpectra.head.duration, newSpectra, 4096, 2048)

    //val spectrogram = signal.segmentToWindows(4096, 2048).toZeroPaddedSpectrogram
    spectrogram.spectra.map(processSpectrum)
  }

  override def spectrogramSamplingRate(samplingRate: Double) = samplingRate / 2048

  private def processSpectrum(spectrum: Spectrum[Signal]): Seq[(Frequency, Double)] = {
    val hsp = computeHSP(spectrum, harmonicsCount)

    val startBin = 0 //52 * spectrum.maxFrequency / spectrum.bandsCount
    val maxIndex = hsp.zipWithIndex.filter(_._2 > startBin).maxBy(_._1)._2
    val maxFrequency = maxIndex * spectrum.bandWidth

    Seq((maxFrequency, 1.0))
  }

  def computeHSP(spectrum: Spectrum[Signal], harmonicsCount: Int): IndexedSeq[Signal] = {
    val amplitudes = spectrum.amplitudes
    val resultLength = amplitudes.size / harmonicsCount

    val res = collection.mutable.IndexedSeq.fill(resultLength)(1.0)

    for (i <- 1 to harmonicsCount) {
      val subAmplitudes = amplitudes.take(i * resultLength)
      val iThAmplitudes = subAmplitudes.zipWithIndex.filter(_._2 % i == 0).unzip._1
      prodIndexedSeqs(res, iThAmplitudes)
    }
    res
  }

  private def prodIndexedSeqs(s1: collection.mutable.IndexedSeq[Signal], s2: IndexedSeq[Signal]) {
    for (i <- 0 until s1.size) s1(i) *= s2(i)
  }

}
