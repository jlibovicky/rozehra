package cz.rozehra.signalProcessing

import cz.rozehra.signalProcessing._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith
import org.scalatest.FunSuite
import java.io._
import scalala.library.Plotting



@RunWith(classOf[JUnitRunner])
class WaveFileTest extends FunSuite {
  test("loading file works") {
    //val resource: InputStream = getClass().getClassLoader().getResourceAsStream("secondOf440.wav");
    //val wave = new WaveFileReader(resource);
    //println(wave.toString)
    //wave.plot
  }

/*  test("spectrogram") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    val wave = new WaveFileReader(resource);
    val spectrogram = wave.segmentToWindows(1024, 512).toSpectrogram

    val visualizer = new Visualizer()
    visualizer.drawSpectrum(spectrogram)
    visualizer.drawNoteOnsets(scala.collection.JavaConversions.asJavaList(Seq(1.0, 3.0).asInstanceOf[Seq[java.lang.Double]]))

    //val spectrum = spectrogram.spectra(17)
    //val specVis = new XYVisualizer("spectrum", spectrum.bandWidth, "Frequency [Hz]", "Power",
    //  scala.collection.JavaConversions.asJavaList(spectrum.amplitudes.toSeq.asInstanceOf[Seq[java.lang.Double]]))
    readLine()

    //spectrogram.plot
    //readLine()
  }*/

  /*test("signal energy") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    val wave = new WaveFileReader(resource);
    val energy = wave.segmentToWindows(64, 32).toEnergy
    //energy.plot
    //readLine()
  } */

}/*  test("windows are created") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("secondOf440.wav");
    val wave = new WaveFileReader(resource);   
    val windows = wave.segmentToWindows(1024)
    windows(0).plot
  }
  
  test("test spectrum is computed") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("secondOf440.wav");
    val wave = new WaveFileReader(resource);   
    val windows = wave.segmentToWindows(1024)
    val spectrum = SpectrumFactory.computeSpectrum[Signal](windows(0))
    spectrum.plot
  }
  
  test("test spectrogram is computed") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("secondOf440.wav");
    val wave = new WaveFileReader(resource);   
    val windows = wave.segmentToWindows(1024)
    val spectrogram = SpectrumFactory.computeSpectrogram[Signal](windows)
    spectrogram.plot
  }
  
  test("generate window back again") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("secondOf440.wav");
    val wave = new WaveFileReader(resource);   
    val windows = wave.segmentToWindows(1024)
    val spectrum = SpectrumFactory.computeSpectrum[Signal](windows(0))
    val regeneratedWindow = spectrum.generateWindow
    regeneratedWindow.plot
  }
  
  test("FFT on fake signal") {
    val sine = (0 until 1024).map(x => Math.sin(x))
    val window = new Window[Signal](44100, sine.toIndexedSeq)
    window.plot
    SpectrumFactory.computeSpectrum[Signal](window).plot
  }
}       */