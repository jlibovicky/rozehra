package cz.rozehra.signalProcessing.pitchDetection

import cz.rozehra.signalProcessing._

object PeakDetection extends FundamentalsDetection {
  def detectFundamentals(signal: TimeDomainWaveForm[Signal]): List[Seq[(Frequency, Double)]] = {
    val spectrogram = signal.segmentToWindows(1024, 512).toSpectrogram
    spectrogram.spectra.map( _.findPeaks(0.172).toSeq )
  }

  def spectrogramSamplingRate(signalSamplingRate: Double): Double = signalSamplingRate / 512
}
