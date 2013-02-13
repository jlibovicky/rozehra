package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.{PartialTrackingForFundamentals, WaveFileReader}

object Run {
 def main(args: Array[String]) {
   val readFileStart = System.currentTimeMillis
   val wave = new WaveFileReader("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01.wav")
   val readFileEnd = System.currentTimeMillis
   println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

   val spectrogram = wave.segmentToWindows(1024, 512).toSpectrogram
   val spectrogramComputed = System.currentTimeMillis
   val extendedSpectrogram = spectrogram //wave.segmentToWindows(4096, 2048).toSpectrogram
   println("Spectrogram computed: " + (spectrogramComputed - readFileEnd) / 1000.0 + " s")


   val peaksStart = System.currentTimeMillis
   val detectedPeaks = extendedSpectrogram.spectra.map( _.findPeaks(0.3).toSeq)
   val fundamentalsEnd = System.currentTimeMillis
   println("Peaks detection: " + (fundamentalsEnd - peaksStart) / 1000.0 + " s")

   val tracks = PartialTrackingForFundamentals.partialTracking(detectedPeaks)
   val trackingEnd = System.currentTimeMillis
   println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")

   val searchingStart = System.currentTimeMillis()
   val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks, spectrogram.spectrumRate))
   println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
   println(result.size)
 }
}
