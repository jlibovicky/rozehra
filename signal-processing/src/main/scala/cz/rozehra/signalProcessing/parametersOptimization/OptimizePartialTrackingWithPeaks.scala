package cz.rozehra.signalProcessing.parametersOptimization

import java.io.{FileInputStream, File}
import cz.rozehra.signalProcessing.{WaveFileReader, Frequency}
import cz.rozehra.signalProcessing.salienceFunction.{FundamentalDetection, Whitening}
import cz.rozehra.signalProcessing.partialtracking.Track
import scala.io.Source
import scala.math._


object OptimizePartialTrackingWithPeaks extends OptimizePartialTracking {
  val toneTolerances: Seq[Double] = Seq(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  val maximumWithoutUpdateValues: Seq[Int] = Seq(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  val minimumTrackDensities: Seq[Double] = Seq(0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  val minimumTrackDurations: Seq[Int] = Seq(1, 2, 3, 4, 5, 6, 7)


  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

    //  list of sets of fundamentals <|> correct solutions
    var listsOfPeaks = Seq.empty[(List[Seq[(Frequency, Double)]], Seq[Double])]
    var samplingRate = 0.0
    for (file <- files.take(1)) {
      System.err.print("File " + file.getName + " ... ")
      val solution = loadCorrectSolution(file)

      val wave = new WaveFileReader(new FileInputStream(file))
      val spectrogram = wave.segmentToWindows(4096, 2048).toSpectrogram
      samplingRate = spectrogram.spectrumRate
      val detectedPeaks = spectrogram.spectra.map( _.findPeaks(0.3).toSeq )
      listsOfPeaks :+= (detectedPeaks, solution)
      System.err.println("loaded")
    }
    val preprocessedFiles = listsOfPeaks.par

    println("EVALUATION OF PEAK DETECTION")
    val (pitchSum, noteSum) = (for ( (fundamentals, solution) <- preprocessedFiles )
    yield evaluatePeakDetection(fundamentals, solution, samplingRate)).
      foldLeft((0.0, 0.0))( (sums, res) => (sums._1 + res._1, sums._2 + res._2))
    println("frequency based accuracy = " + pitchSum / preprocessedFiles.size)
    println("note based accuracy = " + noteSum / preprocessedFiles.size)

    sys.exit()
    println()
    println("PARTIAL TRACKING OPTIMIZATION")
    println("tone tolerance,maximum steps without update,minimum track density,minimum track duration,pitch precision," +
      "note precision,pitch recall,note recall,pitch F-measure,note F-measure")
    for (toneTolerance <- toneTolerances;
         maximumWithoutUpdate <- maximumWithoutUpdateValues;
         minimumTrackDensity <- minimumTrackDensities;
         minimumTrackDuration <- minimumTrackDurations) {
      print(toneTolerance + "," + maximumWithoutUpdate + "," + minimumTrackDensity + "," + minimumTrackDuration + ",")

      val partialTracking = new parametrizedPartialTracking(toneTolerance, maximumWithoutUpdate,
        minimumTrackDensity, minimumTrackDuration)

      val scores = preprocessedFiles.map(
        p => evaluateTracks(partialTracking.partialTracking(p._1), p._2, samplingRate))
      val scoreSum = scores.foldLeft((0.0, 0.0, 0.0, 0.0))((s, v) =>
        (s._1 + v._1, s._2 + v._2, s._3 + v._3, s._4 + v._4) )

      val pitchScore = scoreSum._1 / preprocessedFiles.size
      val noteScore = scoreSum._2 / preprocessedFiles.size

      val pitchRecall = scoreSum._3 / preprocessedFiles.size
      val noteRecall = scoreSum._4 / preprocessedFiles.size

      val pitchFMeasure = 2 * pitchScore * pitchRecall / (pitchScore + pitchRecall)
      val noteFMeasure = 2 * noteScore * noteRecall / (noteScore + noteRecall)

      println(pitchScore + "," + noteScore + "," + pitchRecall + "," + noteRecall + "," +
        pitchFMeasure + "," + noteFMeasure)
    }
  }

  private def evaluatePeakDetection(fundamentals: List[Seq[(Frequency, Double)]], solution: Seq[Double],
                                       samplingRate: Double): (Double, Double)  = {
    val funds: IndexedSeq[Seq[(Frequency, Double)]] = fundamentals.toIndexedSeq

    var rightFrequencyDetected = 0.0
    var rightToneDetected = 0.0

    for ( (frequency, time) <- solution zip (0 to solution.size).map(_ * 0.01) ) {
      val timeIndex = min(floor(time * samplingRate).asInstanceOf[Int], funds.size - 1)

      if ((frequency == 0.0) ||
        funds(timeIndex).exists( f => abs(toneFromFreq(f._1) - toneFromFreq(frequency)) < 0.25))
        rightFrequencyDetected += 1.0

      if ((frequency == 0.0) ||
        funds(timeIndex).exists( f => abs(toneFromFreq(f._1) % 12 - toneFromFreq(frequency) % 12) < 0.25))
        rightToneDetected += 1.0
    }
    (rightFrequencyDetected / solution.size, rightToneDetected / solution.size)
  }
}


