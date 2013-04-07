package cz.rozehra.signalProcessing.pitchDetection.klapuriWhitening

import cz.rozehra.signalProcessing._
import cz.rozehra.signalProcessing.pitchDetection.FundamentalsDetection

object KlapuriFundamentalDetection extends FundamentalsDetection {
  override def detectFundamentals(signal: TimeDomainWaveForm[Signal]) = {
    val originalSamplingRate = signal.samplingRate
    val whitenedSpectrogram = Whitening.whitenSpectrogram(signal)

    whitenedSpectrogram.spectra.par.map(new KlapuriFundamentalsDetector(_, originalSamplingRate).findFundamentals).toList
  }

  override def spectrogramSamplingRate(signalSamplingRate: Double) = signalSamplingRate / Whitening.windowShift
}
