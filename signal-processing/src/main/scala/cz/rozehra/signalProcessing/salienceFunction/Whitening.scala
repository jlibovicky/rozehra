package cz.rozehra.signalProcessing.salienceFunction

import cz.rozehra.signalProcessing._

object Whitening {
  def whitenSpectrogram(spectrogram: Spectrogram[Signal]) = {
    val newSpectra = spectrogram.spectra.par.map( spectrum => new SpectrumWhitener(spectrum).getWhitenedSpectrum ).toList
    new Spectrogram[Signal](spectrogram.spectrumRate, newSpectra,
      spectrogram.signalWindowSize, spectrogram.signalWindowShift)
  }
}
