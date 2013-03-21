package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.trackSelection.{TrackSelection, Hypothesis}
import math._
import cz.rozehra.signalProcessing.{PartialTrackingForFundamentals, WaveFileReader}
import cz.rozehra.signalProcessing.fundamentalsDetection.klapuriWhitening.{KlapuriFundamentalDetection, Whitening}
import java.io.File
import cz.rozehra.signalProcessing.fundamentalsDetection.FundamentalsDetection
import cz.rozehra.signalProcessing.partialtracking.{GenericPartialTracking, PartialTrackingForCBHSB, Track}
import cz.rozehra.signalProcessing.voiceDetection.VoiceDetection
import cz.rozehra.signalProcessing.Signal

trait OptimizeTrackSearchBase extends OptimizeFrequenciesBase {
  val fundamentalsAlgorithm: FundamentalsDetection
  val partialTrackingAlgorithm: GenericPartialTracking[Signal]

  def testAFile(fileName: String): (Double, Double, Double, Double) = {
    val tracksRes = getPartialTracks(fileName)
    testJustTracks(tracksRes._1, tracksRes._2, fileName)
  }

  def getPartialTracks(fileName: String): (Set[Track], Double) = {
    val readFileStart = System.currentTimeMillis
    val wave = new WaveFileReader(fileName)
    val readFileEnd = System.currentTimeMillis
    System.err.println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

    val fundamentalsStart = System.currentTimeMillis
    val detectedFundamentals = fundamentalsAlgorithm.detectFundamentals(wave.toTimeDomainWaveForm)
    val fundamentalsEnd = System.currentTimeMillis
    System.err.println("Fundamentals detection: " + (fundamentalsEnd - fundamentalsStart) / 1000.0 + " s")

    /*val voicing = VoiceDetection.detectVoicing(wave.toTimeDomainWaveForm)
    val voicedFundamentals = (detectedFundamentals zip voicing).map(p =>
      if (p._2) p._1 else Seq.empty)*/

    /*val peaksStart = System.currentTimeMillis
    val detectedPeaks = extendedSpectrogram.spectra.map( _.findPeaks(0.3).toSeq)
    val fundamentalsEnd = System.currentTimeMillis
    println("Peaks detection: " + (fundamentalsEnd - peaksStart) / 1000.0 + " s")*/

    val tracks = partialTrackingAlgorithm.partialTracking(detectedFundamentals)
    val trackingEnd = System.currentTimeMillis
    System.err.println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")
    (tracks, wave.samplingRate)
  }

  def testJustTracks(tracks: Set[Track], samplingRate: Double, fileName: String): (Double, Double, Double, Double) = {
    val searchingStart = System.currentTimeMillis()
    val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks,
      fundamentalsAlgorithm.spectrogramSamplingRate(samplingRate)))
    System.err.println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")
    System.err.println(result.head)
    System.err.println(result.head.scorePerNote)

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
      val note = if (currentNoteIndex >= notes.size) null
                 else notes(currentNoteIndex)

      // silence correctly detected
      if (solution == 0 && (note == null || note.start > time)) { voicingCorrect += 1; silenceCorrect += 1 }
      else if (solution != 0) {
        totalVoicedSegments += 1 // it is a voiced segment
        if (note != null && time > note.start && time < note.end) {
          voicingCorrect += 1
          if ( abs(toneFromFreq(solution) - note.pitch.asInstanceOf[Double]) <= 0.5) { pitchCorrect += 1}
          if ( abs(toneFromFreq(solution) - note.pitch.asInstanceOf[Double]) % 12 <= 0.5 ) { toneCorrect += 1}
        }
      }
    }

    // Voicing Accuracy
    // Raw Pitch Accuracy
    // Raw Chroma Accuracy
    // Overall Accuracy
    ( voicingCorrect.toDouble / solution.size,
      pitchCorrect.toDouble / totalVoicedSegments,
      toneCorrect.toDouble / totalVoicedSegments,
      (silenceCorrect + pitchCorrect).toDouble / solution.size)
  }
}
