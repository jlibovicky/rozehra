package cz.rozehra.signalProcessing.fundamentalsDetection.klapuri

import cz.rozehra.signalProcessing.{Spectrogram, Signal}
import cz.rozehra.signalProcessing.fundamentalsDetection.FundamentalsDetection

object KlapuriFundamentalDetection extends FundamentalsDetection {
  override def detectFundamentals(spectrogram: Spectrogram[Signal], originalSamplingRate: Double) = {
    spectrogram.spectra.par.map(new KlapuriFundamentalsDetector(_, originalSamplingRate).findFundamentals).toList
  }
}
