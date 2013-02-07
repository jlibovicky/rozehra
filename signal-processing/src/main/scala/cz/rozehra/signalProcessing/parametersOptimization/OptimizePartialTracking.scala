package cz.rozehra.signalProcessing.parametersOptimization

import java.io.{FileInputStream, File}
import cz.rozehra.signalProcessing.{WaveFileReader, Frequency}
import cz.rozehra.signalProcessing.salienceFunction.{FundamentalDetection, Whitening}
import cz.rozehra.signalProcessing.partialtracking.Track
import scala.io.Source
import scala.math._

object OptimizePartialTracking {
  val toneTolerances: Seq[Double] = Seq(0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
  val maximumWithoutUpdateValues: Seq[Int] = Seq(2, 3, 4, 5, 6, 7)
  val minimumTrackDensities: Seq[Double] = Seq(0.4, 0.5, 0.6, 0.7, 0.8)
  val minimumTrackDurations: Seq[Int] = Seq(2, 3, 4, 5, 6, 7)


  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

                                     //  list of sets of fundamentals <|> correct solutions
    var listsOfFundamentals = Seq.empty[(List[Seq[(Frequency, Double)]], Seq[Double])]
    var samplingRate = 0.0
    for (file <- files) {
      System.err.print("File " + file.getName + " ... ")
      val solution = loadCorrectSolution(file)

      val wave = new WaveFileReader(new FileInputStream(file))
      val spectrogram = wave.segmentToWindows(4096, 2048).toZeroPaddedSpectrogram
      samplingRate = spectrogram.spectrumRate
      val whitenedSpectrogram = Whitening.whitenSpectrogram(spectrogram)
      val detectedFundamentals = FundamentalDetection.detectFundamentals(whitenedSpectrogram)
      listsOfFundamentals :+= (detectedFundamentals, solution)
      System.err.println("loaded")
    }
    val preprocessedFiles = listsOfFundamentals.par

    println("EVALUATION OF FUNDAMENTALS DETECTION")
    val (pitchSum, noteSum) = (for ( (fundamentals, solution) <- preprocessedFiles )
      yield evaluateSalienceFunction(fundamentals, solution, samplingRate)).
        foldLeft((0.0, 0.0))( (sums, res) => (sums._1 + res._1, sums._2 + res._2))
    println("frequency based accuracy = " + pitchSum / preprocessedFiles.size)
    println("note based accuracy = " + noteSum / preprocessedFiles.size)


    println()
    println("PARTIAL TRACKING OPTIMIZATION")
    println("tone tolerance,maximum steps without update,minimum track density,minimum track duration,pitch score,note score")
    for (toneTolerance <- toneTolerances;
         maximumWithoutUpdate <- maximumWithoutUpdateValues;
         minimumTrackDensity <- minimumTrackDensities;
         minimumTrackDuration <- minimumTrackDurations) {
      print(toneTolerance + "," + maximumWithoutUpdate + "," + minimumTrackDensity + "," + minimumTrackDuration + ",")

      val partialTracking = new parametrizedPartialTracking(toneTolerance, maximumWithoutUpdate,
        minimumTrackDensity, minimumTrackDuration)

      val scores = preprocessedFiles.map(
        p => evaluateTracks(partialTracking.partialTracking(p._1), p._2, samplingRate))
      val scoreSum = scores.foldLeft((0.0, 0.0))((s, v) => (s._1 + v._1, s._2 + v._2) )
      val pitchScore = scoreSum._1 / preprocessedFiles.size
      val noteScore = scoreSum._2 / preprocessedFiles.size
      println(pitchScore + "," + noteScore)
    }
  }

  private def loadCorrectSolution(waveFile: File): Seq[Double] = {
    val solutionFile = Source.fromFile(waveFile.getAbsolutePath.replaceFirst("\\.wav", "REF.txt"))
    var frequencies = Seq.empty[Double]
    for (line <- solutionFile.getLines()) {
       val frequency: String = line.split('\t')(1)
       frequencies :+= frequency.toDouble
    }
    frequencies
  }

  private def evaluateTracks(tracks: Set[Track], solution: Seq[Double], samplingRate: Double): (Double, Double) = {
    var containsCorrectTrack = 0.0
    var containsCorrectOctave = 0.0

    for ( (frequency, time) <- solution zip (0 to solution.size).map(_ * 0.01) ) {
      val timeIndex = time * samplingRate
      val trackOnTime = tracks.filter( t => t.start <= timeIndex && t.end >= timeIndex)

      if ((frequency == 0) || // silent place
        (trackOnTime.exists( t => abs(toneFromFreq(t.averageFrequency) - toneFromFreq(frequency)) < 0.25 )))
        containsCorrectTrack += 1.0

      if ((frequency == 0) || // silent place
        (trackOnTime.exists( t => abs(toneFromFreq(t.averageFrequency) % 12.0 - toneFromFreq(frequency) % 12.0)  < 0.25 )))
        containsCorrectOctave += 1.0
    }
    (containsCorrectTrack / solution.size, containsCorrectOctave / solution.size)
  }

  private def evaluateSalienceFunction(fundamentals: List[Seq[(Frequency, Double)]], solution: Seq[Double],
                                        samplingRate: Double): (Double, Double)  = {
    val funds: IndexedSeq[Seq[(Frequency, Double)]] = fundamentals.toIndexedSeq

    var rightFrequencyDetected = 0.0
    var rightToneDetected = 0.0

    for ( (frequency, time) <- solution zip (0 to solution.size).map(_ * 0.01) ) {
      val timeIndex = floor(time * samplingRate).asInstanceOf[Int]

      if ((frequency == 0.0) ||
        funds(timeIndex).exists( f => abs(toneFromFreq(f._1) - toneFromFreq(frequency)) < 0.25))
        rightFrequencyDetected += 1.0

      if ((frequency == 0.0) ||
        funds(timeIndex).exists( f => abs(toneFromFreq(f._1) % 12 - toneFromFreq(frequency) % 12) < 0.25))
        rightToneDetected += 1.0
    }
    (rightFrequencyDetected / solution.size, rightToneDetected / solution.size)
  }

  private def toneFromFreq(frequency: Double): Double = 69.0 + 12 * log(frequency / 440.0) / log(2.0)
}
