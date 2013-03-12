package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.trackSelection.{TrackSelection, Hypothesis}
import math._
import cz.rozehra.signalProcessing.{PartialTrackingForFundamentals, WaveFileReader}
import cz.rozehra.signalProcessing.fundamentalsDetection.klapuriWhitening.{KlapuriFundamentalDetection, Whitening}
import java.io.File
import cz.rozehra.signalProcessing.fundamentalsDetection.FundamentalsDetection

trait OptimizeTrackSearchBase extends OptimizeFrequenciesBase {
  val fundamentalsAlgorithm: FundamentalsDetection

  def testAFile(fileName: String): (Double, Double, Double, Double) = {
    val readFileStart = System.currentTimeMillis
    val wave = new WaveFileReader(fileName)
    val readFileEnd = System.currentTimeMillis
    System.err.println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

    val fundamentalsStart = System.currentTimeMillis
    val detectedFundamentals = fundamentalsAlgorithm.detectFundamentals(wave.toTimeDomainWaveForm)
    val fundamentalsEnd = System.currentTimeMillis
    System.err.println("Fundamentals detection: " + (fundamentalsEnd - fundamentalsStart) / 1000.0 + " s")

    /*val peaksStart = System.currentTimeMillis
    val detectedPeaks = extendedSpectrogram.spectra.map( _.findPeaks(0.3).toSeq)
    val fundamentalsEnd = System.currentTimeMillis
    println("Peaks detection: " + (fundamentalsEnd - peaksStart) / 1000.0 + " s")*/

    val tracks = PartialTrackingForFundamentals.partialTracking(detectedFundamentals)
    val trackingEnd = System.currentTimeMillis
    System.err.println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")

    val searchingStart = System.currentTimeMillis()
    val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks,
      fundamentalsAlgorithm.spectrogramSamplingRate(wave.samplingRate)))
    System.err.println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
    System.err.println(result.size)

    val solution = loadCorrectSolution(new File(fileName))
    evaluateHypothesis(result.head, solution)
  }

  def evaluateHypothesis(hypothesis: Hypothesis, solution: Seq[Double]): (Double, Double, Double, Double) = {
    val notes = hypothesis.notes.toIndexedSeq

    var currentNoteIndex = 0

    var totalVoicedSegments = 0
    var pitchCorrect = 0
    var toneCorrect = 0
    var voicingCorrect = 0
    var silenceCorrect = 0

    for ((solution, time) <- solution zip (0 to solution.size).map(_ / 100.0)) {
      if (currentNoteIndex < notes.size && notes(currentNoteIndex).end < time) currentNoteIndex += 1
      val note = notes(min(currentNoteIndex, notes.size - 1))

      // silence correctly detected
      if (solution == 0 && note.start > time) { voicingCorrect += 1; silenceCorrect += 1 }
      else {
        totalVoicedSegments += 1 // it is a voiced segment
        if (time > note.start && time < note.end) {
          voicingCorrect += 1
          if ( abs(toneFromFreq(solution) - note.pitch.asInstanceOf[Double]) < 0.45) { pitchCorrect += 1}
          if ( abs(toneFromFreq(solution) % 12 - note.pitch.asInstanceOf[Double] % 12) < 0.45 ) { toneCorrect += 1}
        }
      }
    }

    // Voicing Accuracy
    // Raw Pitch Accuracy
    // Raw Chroma Accuracy
    // Overall Accuracy
    ( voicingCorrect.asInstanceOf[Double] / solution.size,
      pitchCorrect.asInstanceOf[Double] / totalVoicedSegments,
      toneCorrect.asInstanceOf[Double] / totalVoicedSegments,
      (silenceCorrect + pitchCorrect).asInstanceOf[Double] / solution.size)
  }
}
