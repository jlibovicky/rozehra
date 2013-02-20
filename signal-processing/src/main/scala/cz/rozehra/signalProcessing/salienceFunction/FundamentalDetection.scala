package cz.rozehra.signalProcessing.salienceFunction

import cz.rozehra.signalProcessing.{Spectrogram, Signal}

object FundamentalDetection {
  def detectFundamentals(spectrogram: Spectrogram[Signal], originalSamplingRate: Double) = {
    //spectrogram.spectra.map(new FundamentalsDetector(_).findFundamentals).toList
    spectrogram.spectra.par.map(new FundamentalsDetector(_, originalSamplingRate).findFundamentals).toList
  }
}
