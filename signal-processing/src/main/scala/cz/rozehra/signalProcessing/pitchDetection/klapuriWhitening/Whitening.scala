package cz.rozehra.signalProcessing.pitchDetection.klapuriWhitening

import cz.rozehra.signalProcessing._

object Whitening {
  val windowSize = 4096
  val paddedWindowSize = 8192
  val windowShift = 2048

  def whitenSpectrogram(signal: TimeDomainWaveForm[Signal]) = {
    val newSpectra = signal.segmentToWindows(windowSize, windowShift).windows.
      map(w => w.hanningWindow.changeMaxFrameSize(paddedWindowSize)).map(new FreqDomainWindow(_)).
      map(fw => new SpectrumWhitener(fw).getWhitenedSpectrum)

    new Spectrogram[Signal](1 / newSpectra.head.duration, newSpectra, windowSize, windowShift)
  }

  def whitenedFreqDomain(signal: TimeDomainWaveForm[Signal]) = {
    signal.segmentToWindows(4096, 2048).windows.
      map(w => w.hanningWindow.changeMaxFrameSize(8192)).map(new FreqDomainWindow(_)).
      map(fw => new SpectrumWhitener(fw).getWhitenedFreqDomainWindow)
  }
}
