package cz.rozehra.signalProcessing

import fundamentalsDetection.klapuri.{KlapuriFundamentalDetection, Whitening}
import org.scalatest.FunSuite
import trackSelection.TrackSelection
import visualization.Visualizer
import collection.JavaConversions

class TestSearching extends FunSuite {
  test("partial tracking with fundamentals detection") {
    val readFileStart = System.currentTimeMillis
    //val wave = new WaveFileReader(resource);
    val wave = new WaveFileReader("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01.wav")
    val readFileEnd = System.currentTimeMillis
    println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

    val spectrogram = wave.segmentToWindows(1024, 512).toSpectrogram
    val spectrogramComputed = System.currentTimeMillis
    println("Spectrogram computed: " + (spectrogramComputed - readFileEnd) / 1000.0 + " s")
    //spectrogram.plot

    val extendedSpectrogram = wave.segmentToWindows(4096, 2048).toZeroPaddedSpectrogram
    val whitenedSpectrogram = Whitening.whitenSpectrogram(extendedSpectrogram)
    val signalWhitened = System.currentTimeMillis
    println("Signal whitening: " + (signalWhitened - spectrogramComputed) / 1000.0 + " s")
    //whitenedSpectrogram.spectra(157).amplitudes.foreach(println)

    val visualizer2 = new Visualizer()
    visualizer2.drawSpectrum(spectrogram)

    val fundamentalsStart = System.currentTimeMillis
    val detectedFundamentals = KlapuriFundamentalDetection.detectFundamentals(whitenedSpectrogram, wave.samplingRate)
    val fundamentalsEnd = System.currentTimeMillis
    println("Fundamentals detection: " + (fundamentalsEnd - fundamentalsStart) / 1000.0 + " s")

    val tracks = PartialTrackingForFundamentals.partialTracking(detectedFundamentals)
    val trackingEnd = System.currentTimeMillis
    println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")
    //tracks.foreach( t => println(t) )

    visualizer2.drawFundamentals(JavaConversions.asJavaList((detectedFundamentals.map(_.map(_._1.asInstanceOf[java.lang.Double])))),
      extendedSpectrogram.spectrumRate)
    visualizer2.drawPartialTracks(JavaConversions.asJavaList(tracks.toSeq), extendedSpectrogram.spectrumRate)

    val searchingStart = System.currentTimeMillis()
    val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks, spectrogram.spectrumRate))
    println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
    println(result.size)

    visualizer2.drawHypothesis(result.head)

    readLine()
  }
}
