package cz.rozehra.signalProcessing.parametersOptimization

import java.io.{FileInputStream, File}
import cz.rozehra.signalProcessing.{WaveFileReader, Frequency}
import cz.rozehra.signalProcessing.partialtracking.Track
import scala.io.Source
import scala.math._
import cz.rozehra.signalProcessing.fundamentalsDetection.klapuriWhitening.{Whitening, KlapuriFundamentalDetection}

object OptimizePartialTrackingWithFundamentals extends OptimizePartialTracking {
  val toneTolerances: Seq[Double] = Seq(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
  val maximumWithoutUpdateValues: Seq[Int] = Seq(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
  val minimumTrackDensities: Seq[Double] = Seq(0.1, 0.2, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)
  val minimumTrackDurations: Seq[Int] = Seq(1, 2, 3, 4, 5, 6, 7)

  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

                                     //  list of sets of peaks <|> correct solutions
    var listsOfFundamentals = Seq.empty[(List[Seq[(Frequency, Double)]], Seq[Double])]
    var samplingRate = 0.0
    for (file <- files.take(1)) {
      System.err.print("File " + file.getName + " ... ")
      val solution = loadCorrectSolution(file)

      val wave = new WaveFileReader(new FileInputStream(file))
      val spectrogram = wave.segmentToWindows(4096, 2048).toZeroPaddedSpectrogram
      samplingRate = spectrogram.spectrumRate
      val whitenedSpectrogram = Whitening.whitenSpectrogram(spectrogram)
      val detectedFundamentals = KlapuriFundamentalDetection.detectFundamentals(whitenedSpectrogram, wave.samplingRate)
      listsOfFundamentals :+= (detectedFundamentals, solution)
      System.err.println("loaded")
    }
    val preprocessedFiles = listsOfFundamentals.par

    println("EVALUATION OF FUNDAMENTALS DETECTION")
    val (pitchSum, noteSum, vPitchSum, vNoteSum) = (for ( (fundamentals, solution) <- preprocessedFiles )
      yield evaluateSalienceFunction(fundamentals, solution, samplingRate)).
        foldLeft((0.0, 0.0, 0.0, 0.0))( (sums, res) =>
          (sums._1 + res._1, sums._2 + res._2, sums._3 + res._3, sums._4 + res._4))
    println("frequency based recall = " + pitchSum / preprocessedFiles.size)
    println("note based recall = " + noteSum / preprocessedFiles.size)
    println("voiced segments frequency recall = " + vPitchSum / preprocessedFiles.size )
    println("voiced segments tone recall = " + vNoteSum / preprocessedFiles.size)

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

      val pitchRecall = scoreSum._1 / preprocessedFiles.size
      val noteRecall = scoreSum._2 / preprocessedFiles.size

      val pitchPrecision = scoreSum._3 / preprocessedFiles.size
      val notePrecision = scoreSum._4 / preprocessedFiles.size

      val pitchFMeasure = 2 * pitchRecall * pitchPrecision / (pitchRecall + pitchPrecision)
      val noteFMeasure = 2 * noteRecall * notePrecision / (noteRecall + notePrecision)

      println(pitchRecall + "," + noteRecall + "," + pitchPrecision + "," + notePrecision + "," +
        pitchFMeasure + "," + noteFMeasure)
    }
  }

  protected def evaluateSalienceFunction(fundamentals: List[Seq[(Frequency, Double)]], solution: Seq[Double],
                                        samplingRate: Double): (Double, Double, Double, Double)  = {
    val funds: IndexedSeq[Seq[(Frequency, Double)]] = fundamentals.toIndexedSeq

    var rightFrequencyDetected = 0.0
    var rightFrequenceVoicedDetected = 0.0

    var rightToneDetected = 0.0
    var rightToneVoicedDtectecd = 0.0

    var silentSegments = 0.0

    for ( (frequency, time) <- solution zip (0 to solution.size).map(_ * 0.01) ) {
      val timeIndex = min(floor(time * samplingRate).asInstanceOf[Int], funds.size - 1)

      if (frequency == 0.0) {
        rightFrequencyDetected += 1
        rightToneDetected += 1
        silentSegments += 1
      }

      if (funds(timeIndex).exists( f => abs(toneFromFreq(f._1) - toneFromFreq(frequency)) < 0.25)) {
        rightFrequencyDetected += 1.0
        rightFrequenceVoicedDetected += 1.0
      }

      if (funds(timeIndex).exists( f => abs(toneFromFreq(f._1) % 12 - toneFromFreq(frequency) % 12) < 0.25)) {
        rightToneDetected += 1.0
        rightToneVoicedDtectecd += 1.0
      }
    }
    (rightFrequencyDetected / solution.size, rightToneDetected / solution.size,
      rightFrequenceVoicedDetected / (solution.size - silentSegments), rightToneVoicedDtectecd / (solution.size - silentSegments))
  }
}
