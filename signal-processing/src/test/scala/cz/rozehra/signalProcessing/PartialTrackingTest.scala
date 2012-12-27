package cz.rozehra.signalProcessing

import org.scalatest.FunSuite
import java.io.InputStream
import salienceFunction.{FundamentalDetection, Whitening}
import visualization.Visualizer
import collection.JavaConversions


class PartialTrackingTest extends FunSuite {
  /*test("test spectrum peak detection") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    val wave = new WaveFileReader(resource);
    val spectrogram = wave.segmentToWindows(1024, 512).toSpectrogram
    val testSpectrum = spectrogram.spectra(25)

    val index = 60

    new SpectrumShower(null, index.toString, index, testSpectrum.bandWidth,
      scala.collection.JavaConversions.asJavaList(testSpectrum.amplitudes.toSeq.asInstanceOf[Seq[java.lang.Double]]))

    val medianMultiply = 200.0
    val median = (testSpectrum.amplitudes.sortWith( (e1, e2) => e1 < e2))(testSpectrum.amplitudes.size / 2)
    val filteredSpectrum = new Spectrum[Signal](testSpectrum.withWindowShift, testSpectrum.bandWidth,
      testSpectrum.amplitudes.map( (a: Signal) => if (a > median * medianMultiply) a else 0.0))

    new SpectrumShower(null, index.toString + " - after median filter", index, testSpectrum.bandWidth,
      scala.collection.JavaConversions.asJavaList(filteredSpectrum.amplitudes.toSeq.asInstanceOf[Seq[java.lang.Double]]))

    readLine()
  }   */

  test("signal whitening") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");

    val readFileStart = System.currentTimeMillis
    //val wave = new WaveFileReader(resource);
    val wave = new WaveFileReader("C:\\MFF\\rozehra\\tempo_train_data\\train1.wav")
    val readFileEnd = System.currentTimeMillis
    println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

    val spectrogram = wave.segmentToWindows(1024, 512).toSpectrogram
    val spectrogramComputed = System.currentTimeMillis
    println("Spectrogram computed: " + (spectrogramComputed - readFileEnd) / 1000.0 + " s")
    //spectrogram.plot

    val whitenedSpectrogram = Whitening.whitenSpectrogram(spectrogram)
    val signalWhitened = System.currentTimeMillis
    println("Signal whitening: " + (signalWhitened - spectrogramComputed) / 1000.0 + " s")
    //whitenedSpectrogram.spectra(157).amplitudes.foreach(println)

    val visualizer2 = new Visualizer()
    visualizer2.drawSpectrum(whitenedSpectrogram)

    val fundamentalsStart = System.currentTimeMillis
    val detectedFundamentals = FundamentalDetection.detectFundamentals(whitenedSpectrogram)
    val fundamentalsEnd = System.currentTimeMillis
    println("Fundamentals detection: " + (fundamentalsEnd - fundamentalsStart) / 1000.0 + " s")

    val tracks = PartialTracking.partialTracking(detectedFundamentals)
    val trackingEnd = System.currentTimeMillis
    println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")
    //tracks.foreach( t => println(t) )

    visualizer2.drawFundamentals(JavaConversions.asJavaList((detectedFundamentals.map(_.map(_._1.asInstanceOf[java.lang.Double])))))
    visualizer2.drawPartialTracks(JavaConversions.asJavaList(tracks.toSeq))
    readLine()
  }
}
