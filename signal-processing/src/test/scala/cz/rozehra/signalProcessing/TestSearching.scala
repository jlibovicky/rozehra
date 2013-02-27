package cz.rozehra.signalProcessing

import fundamentalsDetection.klapuriPerceptional.Constants
import fundamentalsDetection.klapuriWhitening.{KlapuriFundamentalDetection, Whitening}
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
    wave.segmentToWindows(4096, 2048).toSpectrogram.gnuplot("normalSpectrogram.png")
    val spectrogramComputed = System.currentTimeMillis
    //spectrogram.gnuplot()
    println("Spectrogram computed: " + (spectrogramComputed - readFileEnd) / 1000.0 + " s")
    //spectrogram.plot

    //val extendedSpectrogram = wave.segmentToWindows(4096, 2048).toZeroPaddedSpectrogram
    val whitenedSpectrogram = Whitening.whitenSpectrogram(wave.toTimeDomainWaveForm)
    //val whitenedSpectrogram = Constants.computePerceptionalSpectrogram(wave.toTimeDomainWaveForm)
    val signalWhitened = System.currentTimeMillis
    whitenedSpectrogram.gnuplot("whitenedSpectrogram.png")
    println("Signal whitening: " + (signalWhitened - spectrogramComputed) / 1000.0 + " s")

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
      whitenedSpectrogram.spectrumRate)
    visualizer2.drawPartialTracks(JavaConversions.asJavaList(tracks.toSeq), whitenedSpectrogram.spectrumRate)

    val searchingStart = System.currentTimeMillis()
    val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks, spectrogram.spectrumRate))
    println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
    println(result.size)

    visualizer2.drawHypothesis(result.head)
    println(result.head)
    //visualizer2.draw

    readLine()
  }
}
