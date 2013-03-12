package cz.rozehra.signalProcessing

import fundamentalsDetection.FundamentalsDetection
import fundamentalsDetection.harmonicSpectrumProduct.{CBHSP, HSP}
import fundamentalsDetection.klapuriWhitening.{SpectrumWhitener, KlapuriFundamentalDetection, Whitening}
import org.scalatest.FunSuite
import trackSelection.TrackSelection
import visualization.Visualizer
import collection.JavaConversions
import io.Source

class TestSearching extends FunSuite {
  /*test("partial tracking with fundamentals detection") {
    val readFileStart = System.currentTimeMillis
    //val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    //val wave = new WaveFileReader(resource);
    val wave = new WaveFileReader("C:\\MFF\\rozehra\\mirex05TrainFiles\\train02.wav")
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

    val solutions = loadCorrectSolution("C:\\MFF\\rozehra\\mirex05TrainFiles\\train02REF.txt")
    visualizer2.drawSolution(JavaConversions.asJavaList(solutions.map(_.asInstanceOf[java.lang.Double])))

    val whitenedSpectrogram = Whitening.whitenSpectrogram(wave.toTimeDomainWaveForm)
    //val whitenedSpectrogram = PerceptionalFundamentalsDetection.computePerceptionalSpectrogram(wave.toTimeDomainWaveForm)
    val signalWhitened = System.currentTimeMillis
    whitenedSpectrogram.gnuplot("whitenedSpectrogram.png")
    println("Signal whitening: " + (signalWhitened - spectrogramComputed) / 1000.0 + " s")

    val fundamentalsStart = System.currentTimeMillis
    val detectedFundamentals = KlapuriFundamentalDetection.detectFundamentals(whitenedSpectrogram, wave.samplingRate)
    val fundamentalsEnd = System.currentTimeMillis
    println("Fundamentals detection: " + (fundamentalsEnd - fundamentalsStart) / 1000.0 + " s")
    visualizer2.drawFundamentals(JavaConversions.asJavaList((detectedFundamentals.map(_.map(_._1.asInstanceOf[java.lang.Double])))),
      whitenedSpectrogram.spectrumRate)

    val tracks = PartialTrackingForFundamentals.partialTracking(detectedFundamentals)
    val trackingEnd = System.currentTimeMillis
    println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")
    //tracks.foreach( t => println(t) )

    visualizer2.drawPartialTracks(JavaConversions.asJavaList(tracks.toSeq), whitenedSpectrogram.spectrumRate)

    val searchingStart = System.currentTimeMillis()
    val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks, whitenedSpectrogram.spectrumRate))
    println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
    println(result.size)

    if (result.nonEmpty) {
      visualizer2.drawHypothesis(result.head)
      println(result.head)
    }

    readLine()
  }    */

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
    val wave = new WaveFileReader("C:\\MFF\\rozehra\\mirex05TrainFiles\\train05.wav")
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

    val solutions = loadCorrectSolution("C:\\MFF\\rozehra\\mirex05TrainFiles\\train05REF.txt")
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
