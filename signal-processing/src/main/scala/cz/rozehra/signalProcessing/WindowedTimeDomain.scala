package cz.rozehra.signalProcessing

class WindowedTimeDomain[T <: Double](val samplingRate: Frequency, 
    val windowSize: Int, val windowShift: Int, val windows: List[Window[T]]) {
    
  def toSpectrogram: Spectrogram[T] = {
    val spectra: List[Spectrum[T]] = windows.map(w => w.toSpectrum)
    new Spectrogram(1 / spectra.head.duration, spectra, windowSize, windowShift)
  }
}