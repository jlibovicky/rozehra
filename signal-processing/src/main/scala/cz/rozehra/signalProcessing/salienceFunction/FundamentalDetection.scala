package cz.rozehra.signalProcessing.salienceFunction

import cz.rozehra.signalProcessing.{Spectrogram, Signal}

object FundamentalDetection {
  def detectFundamentals(spectrogram: Spectrogram[Signal]) = {
    //spectrogram.spectra.map(new FundamentalsDetector(_).findFundamentals).toList
    spectrogram.spectra.par.map(new FundamentalsDetector(_).findFundamentals).toList
  }
}
