package cz.rozehra.signalProcessing.fundamentalsDetection.klapuriWhitening

import cz.rozehra.signalProcessing._

object Whitening {
  def whitenSpectrogram(signal: TimeDomainWaveForm[Signal]) = {
    val newSpectra = signal.segmentToWindows(4096, 2048).windows.
      map(w => w.hanningWindow.changeMaxFrameSize(8192)).map(new FreqDomainWindow(_)).
      map(fw => new SpectrumWhitener(fw).getWhitenedSpectrum)

    new Spectrogram[Signal](1 / newSpectra.head.duration, newSpectra, 4096, 2048)
  }
}
