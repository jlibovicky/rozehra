package cz.rozehra.signalProcessing

class WindowedTimeDomain[T <: Double](val samplingRate: Frequency, 
    val windowSize: Int, val windowShift: Int, val windows: List[Window[T]]) {
    
  def toSpectrogram: Spectrogram[T] = {
    val spectra: List[Spectrum[T]] = windows.map(w => w.toSpectrum)
    new Spectrogram(1 / spectra.head.duration, spectra, windowSize, windowShift)
  }

  def toEnergy: TimeDomainWaveForm[Energy] = {
    if (this.isInstanceOf[Window[Signal]]) throw new Exception("Energy can be computed from singal only.")
    val energySamplingRate = samplingRate / windowShift
    val energySamples = windows.map( w => w.getEnergy ).toIndexedSeq
    new TimeDomainWaveForm[Energy](energySamplingRate, energySamples)
  }
}