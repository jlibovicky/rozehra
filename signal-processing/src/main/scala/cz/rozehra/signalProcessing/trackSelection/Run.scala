package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.{PartialTrackingForFundamentals, WaveFileReader}
import cz.rozehra.signalProcessing.fundamentalsDetection.klapuri.{KlapuriFundamentalDetection, Whitening}

object Run {
 def main(args: Array[String]) {
   val readFileStart = System.currentTimeMillis
   val wave = new WaveFileReader("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01.wav")
   val readFileEnd = System.currentTimeMillis
   println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

   val extendedSpectrogram = wave.segmentToWindows(4096, 2048).toZeroPaddedSpectrogram
   val spectrogramComputed = System.currentTimeMillis
   println("Spectrogram computed: " + (spectrogramComputed - readFileEnd) / 1000.0 + " s")

   val whitenedSpectrogram = Whitening.whitenSpectrogram(extendedSpectrogram)
   val signalWhitened = System.currentTimeMillis
   println("Signal whitening: " + (signalWhitened - spectrogramComputed) / 1000.0 + " s")
   //whitenedSpectrogram.spectra(157).amplitudes.foreach(println)


   val fundamentalsStart = System.currentTimeMillis
   val detectedFundamentals = KlapuriFundamentalDetection.detectFundamentals(whitenedSpectrogram, wave.samplingRate)
   val fundamentalsEnd = System.currentTimeMillis
   println("Fundamentals detection: " + (fundamentalsEnd - fundamentalsStart) / 1000.0 + " s")

   /*val peaksStart = System.currentTimeMillis
   val detectedPeaks = extendedSpectrogram.spectra.map( _.findPeaks(0.3).toSeq)
   val fundamentalsEnd = System.currentTimeMillis
   println("Peaks detection: " + (fundamentalsEnd - peaksStart) / 1000.0 + " s")*/


   val tracks = PartialTrackingForFundamentals.partialTracking(detectedFundamentals)
   val trackingEnd = System.currentTimeMillis
   println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")

   val searchingStart = System.currentTimeMillis()
   val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks, extendedSpectrogram.spectrumRate))
   println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
   println(result.size)
 }
}
