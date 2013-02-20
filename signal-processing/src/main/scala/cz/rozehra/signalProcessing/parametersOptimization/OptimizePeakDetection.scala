package cz.rozehra.signalProcessing.parametersOptimization

import java.io.{FileInputStream, File}
import cz.rozehra.signalProcessing._
import math._

object OptimizePeakDetection extends OptimizeFrequenciesBase {
  val deltas = (0 until 1200).map(_ * 0.001)

  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

                   //  list of sets of peaks <|> correct solutions
    var spectrograms = Seq.empty[(Spectrogram[Signal], Seq[Double])]
    for (file <- files) {
      System.err.print("File " + file.getName + " ... ")
      val solution = loadCorrectSolution(file)

      val wave = new WaveFileReader(new FileInputStream(file))
      val spectrogram = wave.segmentToWindows(1024, 512).toSpectrogram

      spectrograms :+= (spectrogram, solution)
      System.err.println("loaded")
    }
    val preprocessedFiles = spectrograms.par

    println
    println("delta,pitch recall,tone recall,pitch precision,tone precision,pitch F2-measure,tone F2-measure")
    for (delta <- deltas) {
      print(delta + ",")

      val scores = for ((spectrogram: Spectrogram[Signal], solution) <- preprocessedFiles)
        yield evaluatePeakDetection(spectrogram.spectra.map(_.findPeaks(delta)), solution, spectrogram.spectrumRate)

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

  private def evaluatePeakDetection(peaks: List[Set[(Frequency, Double)]], solution: Seq[Double],
                                    samplingRate: Double): (Double, Double, Double, Double)  = {
    val funds: IndexedSeq[Set[(Frequency, Double)]] = peaks.toIndexedSeq

    var rightFrequencyDetected = 0.0
    var rightToneDetected = 0.0

    var rightFrequencyPrecision = 0.0
    var rightTonePrecision = 0.0

    var voicedPlaces = 0.0

    for ( (frequency, time) <- solution zip (0 to solution.size).map(_ * 0.01) ) {
      val timeIndex = min(floor(time * samplingRate).asInstanceOf[Int], funds.size - 1)

      if (frequency != 0.0) voicedPlaces += 1

      val correctlyPitchedPeaks = funds(timeIndex).filter( f => abs(toneFromFreq(f._1) - toneFromFreq(frequency)) < 0.25)
      if ((frequency != 0.0) && correctlyPitchedPeaks.nonEmpty) {
        rightFrequencyDetected += 1.0
        rightFrequencyPrecision += (correctlyPitchedPeaks.size + 0.0) / funds(timeIndex).size
      }

      val correctlyTonedPeaks = funds(timeIndex).filter( f => abs(toneFromFreq(f._1) % 12 - toneFromFreq(frequency) % 12) < 0.25 )
      if ((frequency != 0.0) && correctlyTonedPeaks.nonEmpty) {
        rightToneDetected += 1.0
        rightTonePrecision += (correctlyTonedPeaks.size + 0.0) / funds(timeIndex).size
      }
    }
    (rightFrequencyDetected / voicedPlaces, rightToneDetected / voicedPlaces,
      rightFrequencyPrecision / voicedPlaces, rightTonePrecision / voicedPlaces)
  }
}
