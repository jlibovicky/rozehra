package cz.rozehra.signalProcessing

import fundamentalsDetection.{CombinedFundamentalsDetection, FundamentalsDetection}
import fundamentalsDetection.harmonicSpectrumProduct.{CBHSPwithWhitening, CBHSP, HSP}
import fundamentalsDetection.klapuriWhitening.{SpectrumWhitener, KlapuriFundamentalDetection, Whitening}
import org.scalatest.FunSuite
import trackSelection.TrackSelection
import visualization.Visualizer
import collection.JavaConversions
import io.Source

class TestSearching extends FunSuite {
  private def loadCorrectSolution(solutionFilePath: String): Seq[Double] = {
    val solutionFile = Source.fromFile(solutionFilePath)
    var frequencies = Seq.empty[Double]
    for (line <- solutionFile.getLines()) {
      val frequency: String = line.split('\t')(1)
      frequencies :+= frequency.toDouble
    }
    frequencies
  }

  test("partial tracking with HPS") {
    val fundDetection: FundamentalsDetection = CBHSP
    val readFileStart = System.currentTimeMillis
    //val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    //val wave = new WaveFileReader(resource);
    val wave = new WaveFileReader("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01.wav")
    val readFileEnd = System.currentTimeMillis
    println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

    val spectrogram = wave.segmentToWindows(4096, 2048).toSpectrogram
    wave.segmentToWindows(4096, 2048).toSpectrogram.gnuplot("normalSpectrogram.png")
    val spectrogramComputed = System.currentTimeMillis
    //spectrogram.gnuplot()
    println("Spectrogram computed: " + (spectrogramComputed - readFileEnd) / 1000.0 + " s")
    //spectrogram.plot

    val visualizer2 = new Visualizer()
    visualizer2.drawSpectrum(spectrogram)

    val solutions = loadCorrectSolution("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01REF.txt")
    visualizer2.drawSolution(JavaConversions.asJavaList(solutions.map(_.asInstanceOf[java.lang.Double])))

   /* val paddedSpectrogram = new Spectrogram[Signal](1 / newSpectra.head.duration, newSpectra, 4096, 2048)
    val signalWhitened = System.currentTimeMillis
    paddedSpectrogram.gnuplot("whitenedSpectrogram.png")
    println("Padded spectrogram computation: " + (signalWhitened - spectrogramComputed) / 1000.0 + " s") */

    val spectrumRate = fundDetection.spectrogramSamplingRate(wave.samplingRate)
    val fundamentalsStart = System.currentTimeMillis
    val detectedFundamentals = fundDetection.detectFundamentals(wave.toTimeDomainWaveForm)
    val fundamentalsEnd = System.currentTimeMillis
    println("Fundamentals detection: " + (fundamentalsEnd - fundamentalsStart) / 1000.0 + " s")
    visualizer2.drawFundamentals(JavaConversions.asJavaList((detectedFundamentals.map(_.map(_._1.asInstanceOf[java.lang.Double])))),
      spectrumRate)

    val tracks = PartialTrackingForFundamentals.partialTracking(detectedFundamentals)
    val trackingEnd = System.currentTimeMillis
    println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")
    //tracks.foreach( t => println(t) )

    visualizer2.drawPartialTracks(JavaConversions.asJavaList(tracks.toSeq), spectrumRate)

    val searchingStart = System.currentTimeMillis()
    val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks, spectrumRate))
    println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
    println(result.size)

    if (result.nonEmpty) {
      visualizer2.drawHypothesis(result.head)
      println(result.head)
    }

    readLine()
  }
}
