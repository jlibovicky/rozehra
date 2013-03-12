package cz.rozehra.signalProcessing.fundamentalsDetection.klapuriWhitening

import cz.rozehra.signalProcessing._
import cz.rozehra.signalProcessing.fundamentalsDetection.FundamentalsDetection

object KlapuriFundamentalDetection extends FundamentalsDetection {
  override def detectFundamentals(signal: TimeDomainWaveForm[Signal]) = {
    val originalSamplingRate = signal.samplingRate
    val whitenedSpectrogram = Whitening.whitenSpectrogram(signal)

    whitenedSpectrogram.spectra.par.map(new KlapuriFundamentalsDetector(_, originalSamplingRate).findFundamentals).toList
  }

  override def spectrogramSamplingRate(signalSamplingRate: Double) = signalSamplingRate / Whitening.windowShift
}
