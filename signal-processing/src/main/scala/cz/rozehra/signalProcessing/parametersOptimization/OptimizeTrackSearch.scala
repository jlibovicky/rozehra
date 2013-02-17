package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.{PartialTrackingForFundamentals, WaveFileReader}
import cz.rozehra.signalProcessing.trackSelection.{Hypothesis, TrackSelection}
import java.io.File
import io.Source
import math._
import scala.Predef._

object OptimizeTrackSearch extends OptimizeFrequenciesBase {
    def main(args: Array[String]) {
      val readFileStart = System.currentTimeMillis
      val wave = new WaveFileReader("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01.wav")
      val readFileEnd = System.currentTimeMillis
      println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

      val spectrogram = wave.segmentToWindows(1024, 512).toSpectrogram
      val spectrogramComputed = System.currentTimeMillis
      val extendedSpectrogram = spectrogram //wave.segmentToWindows(4096, 2048).toSpectrogram
      println("Spectrogram computed: " + (spectrogramComputed - readFileEnd) / 1000.0 + " s")

      val peaksStart = System.currentTimeMillis
      val detectedPeaks = extendedSpectrogram.spectra.map( _.findPeaks(0.3).toSeq)
      val fundamentalsEnd = System.currentTimeMillis
      println("Peaks detection: " + (fundamentalsEnd - peaksStart) / 1000.0 + " s")

      val tracks = PartialTrackingForFundamentals.partialTracking(detectedPeaks)
      val trackingEnd = System.currentTimeMillis
      println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")

      val searchingStart = System.currentTimeMillis()
      val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks, spectrogram.spectrumRate))
      println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
      println(result.size)

      val solution = loadCorrectSolution(new File("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01.wav"))
      val score = evaluateHypothesis(result.head, solution)

      println()
      println("Voicing Accuray: " + score._1)
      println("Raw Pitch Accuracy: " + score._2)
      println("Raw Chroma Accuracy: " + score._3)
      println("Overall Accuracy: " + score._4)
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
          if ( abs(toneFromFreq(solution) - note.pitch.asInstanceOf[Double]) < 0.25) { pitchCorrect += 1}
          if ( abs(toneFromFreq(solution) % 12 - note.pitch.asInstanceOf[Double] % 12) < 0.25 ) { toneCorrect += 1}
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
