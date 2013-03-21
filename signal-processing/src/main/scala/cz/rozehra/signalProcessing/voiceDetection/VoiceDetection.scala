package cz.rozehra.signalProcessing.voiceDetection

import cz.rozehra.signalProcessing._

object VoiceDetection {
  val windowSize = 4096
  val windowShift = 2048

  val alpha = 1.0
  val beta = 1.0
  val gamma = 20

  val minFreq = 512.0
  val maxFreq = 2048.0

  def detectVoicing(signal: TimeDomainWaveForm[Signal]): Seq[Boolean] = {
    val BValues = signal.segmentToWindows(4096, 2048).windows.map(BValue)
    val BMean = BValues.foldLeft(0.0)(_ + _) / BValues.size
    BValues.map(b => (b - BMean) > 0)
  }

  private def BValue(window: Window[Signal]): Double = {
    val spectrum = window.changeMaxFrameSize(windowSize * 2).toSpectrum

    val minBin = (minFreq / spectrum.maxFrequency * spectrum.bandsCount).toInt
    val maxBin = (maxFreq / spectrum.maxFrequency * spectrum.bandsCount).toInt

    val filteredAmplitudes = IndexedSeq.fill(minBin)(0.0) ++
      spectrum.amplitudes.take(maxBin).drop(minBin) ++ IndexedSeq.fill(windowSize / 2 - maxBin)(0.0)

    fft.FFT.fft(filteredAmplitudes).take(gamma).map(_.abs).sum
  }

  def windowRate(signalSamplingRate: Double): Double = signalSamplingRate / windowShift
}
