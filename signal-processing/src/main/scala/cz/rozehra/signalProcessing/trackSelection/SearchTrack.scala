package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.{Frequency, Time}
import math._
import org.apache.commons.math3.distribution.NormalDistribution
import java.io.File
import io.Source

class SearchTrack(frequencies: Seq[Frequency], start: Time, end: Time) extends
    AbstractSearchTrack(frequencies, start, end) {
  private val octavePenalty = TrackSelectionParameters.octavePenalty
  private val durationTolerance = TrackSelectionParameters.durationTolerance
  private val timeStep = TrackSelectionParameters.timeStep
  private val durationWeight = TrackSelectionParameters.durationTolerance
  private val lowestNote = TrackSelectionParameters.lowestNote
  private val highestNote = TrackSelectionParameters.highestNote

  def duration = end - start
  val tones = frequencies.map( toneFromFreq )
  def notePossibilities: Seq[(Note, Double)] = {
    val tonesMean = tones.foldLeft(0.0)(_ + _) / tones.size
    val tonesSd = sqrt(tones.foldLeft(0.0)( (s, f) => s + pow(f - tonesMean, 2.0)) / tones.size)

    var scoredPitches = Set.empty[(Int, Double)]
    for (tone <- if (tonesSd > 0) Set(floor(tonesMean).asInstanceOf[Int] - 1, floor(tonesMean).asInstanceOf[Int],
                                   ceil(tonesMean).asInstanceOf[Int], ceil(tonesMean).asInstanceOf[Int] + 1 )
                 else Set(floor(tonesMean).asInstanceOf[Int], ceil(tonesMean).asInstanceOf[Int])) {
      val toneDist = new NormalDistribution(tone, 0.5)
      val score = if (tonesSd == 0) 1 - abs(tone - tonesMean)
                  else toneDist.cumulativeProbability(tonesMean - tonesSd, tonesMean + tonesSd)

      scoredPitches += ((tone, score))
      if (octavePenalty != 0) {
        if (tone > lowestNote + 12) scoredPitches += ((tone - 12, octavePenalty * score))
        if (tone < highestNote - 12) scoredPitches += ((tone + 12, octavePenalty * score))
      }
    }

    val pitchScoreSum = scoredPitches.foldLeft(0.0)(_ + _._2)
    val normedScoredPitches = scoredPitches.map(p => (p._1, p._2 / pitchScoreSum))

    val durationInterval = durationTolerance * (end - start)
    val stepsCount = round(durationInterval / timeStep).toInt

    val startLeftBound = if (stepsCount == 0) round(100.0 * start) / 100.0
                         else round((start - durationInterval / 2) * 100.0) / 100.0
    val startPossibilities = (0 to stepsCount).map(startLeftBound + _ * timeStep)

    val endLeftBound = if (stepsCount == 0) round(100.0 * end) / 100.0
                       else round((end - durationInterval / 2) * 100.0) / 100.0
    val endPossibilities = (0 to stepsCount).map(endLeftBound + _ * timeStep)

    val scoredTimeIntervals = (for (s <- startPossibilities; e <- endPossibilities) yield (s, e)).
      filter( interval => interval._1 < interval._2).
      map( interval => (interval, 1 - abs(duration - interval._1 + interval._2) / duration ) )

    val totalDurationScore = scoredTimeIntervals.foldLeft(0.0)(_ + _._2)
    val normedScoredTimeIntervals = scoredTimeIntervals.map(p => (p._1, p._2 / totalDurationScore))

    (for ( (pitch, pitchScore) <- normedScoredPitches; (interval, durationScore) <- normedScoredTimeIntervals)
      yield (new Note(pitch, interval._1, interval._2), pitchScore * pow(durationScore, durationWeight))).toSeq.
        sortBy(_._2)
  }

  private def toneFromFreq(frequency: Double): Double = 69.0 + 12 * log(frequency / 440.0) / log(2.0)
}
