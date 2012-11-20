package cz.rozehra.signalProcessing.tempo

import cz.rozehra.signalProcessing.Spectrum
import cz.rozehra.signalProcessing.Spectrogram

object NoteOnsetDetection {
    def sliceSpectrum[T <: Double](spectrum : Spectrum[T]): List[Spectrum[T]] = {
      val spectrumLength = spectrum.amplitudes.length
      
      val zeros = IndexedSeq.fill[T](spectrumLength)(0.0.asInstanceOf[T])
      def iterate(amplitudes: List[T], i: Int, accu: List[Spectrum[T]]): List[Spectrum[T]] =
        amplitudes match {
          case Nil => accu.reverse
          case amp :: rest => 
            iterate(rest, i + 1, 
                    new Spectrum[T](spectrum.duration, spectrum.bandWidth, 
                        IndexedSeq.fill[T](i)(0.0.asInstanceOf[T]) ++ IndexedSeq(amp) ++ 
                        IndexedSeq.fill[T](spectrumLength - i -1)(0.0.asInstanceOf[T])) :: accu)
      }
      
      iterate(spectrum.amplitudes.toList, 0, Nil)
    }
    
    def sliceSpectrogram[T <: Double](spectrogram: Spectrogram[T]): List[Spectrogram[T]] = {
      val slicedSpectra = spectrogram.spectra.map(s => sliceSpectrum[T](s))
      val nils = List.fill[List[Spectrum[T]]](spectrogram.spectra.length)
      
      Nil
    }
}