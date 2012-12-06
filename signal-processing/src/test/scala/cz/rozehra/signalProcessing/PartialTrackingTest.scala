package cz.rozehra.signalProcessing

import org.scalatest.FunSuite
import java.io.InputStream
import scalala.library.Plotting
import scalala.tensor.dense.DenseMatrix
import java.awt.Color
import scalala.library.plotting.GradientPaintScale


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

  /*test("test spectrum peak detection") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    val wave = new WaveFileReader(resource);
    val spectrogram = wave.segmentToWindows(1024, 512).toSpectrogram
    //spectrogram.plot

    val visualzer = new Visualizer()
    visualzer.drawSpectrum(spectrogram)

    val tracks = PartialTracking.partialTracking(spectrogram.spectra)
    tracks.foreach( t => println(t) )


    visualzer.drawPartialTracks(scala.collection.JavaConversions.asJavaList(tracks.toSeq))
    readLine()
  }*/
}
