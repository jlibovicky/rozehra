package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.{Frequency, Time}
import math._
import org.apache.commons.math3.distribution.NormalDistribution

class SearchTrack(frequencies: Seq[Frequency], start: Time, end: Time) extends
    AbstractSearchTrack(frequencies, start, end) {
  private val octavePenalty = TrackSelectionParameters.octavePenalty
  private val durationTolerance = TrackSelectionParameters.durationTolerance
  private val timeStep = TrackSelectionParameters.durationTolerance
  private val durationWeight = TrackSelectionParameters.durationTolerance

  def duration = end - start
  val tones = frequencies.map( toneFromFreq )
  def notePossibilities: Seq[(Note, Double)] = {
    val tonesMean = tones.foldLeft(0.0)(_ + _) / tones.size
    val tonesSd = sqrt(tones.foldLeft(0.0)( (s, f) => s + pow(f - tonesMean, 2.0)) / tones.size)

    var scoredPitches = Set.empty[(Int, Double)]
    for (tone <- Set(floor(tonesMean).asInstanceOf[Int] - 1, floor(tonesMean).asInstanceOf[Int],
        ceil(tonesMean).asInstanceOf[Int], ceil(tonesMean).asInstanceOf[Int] + 1 )) {
      val toneDist = new NormalDistribution(tone, 0.5)
      val score = toneDist.cumulativeProbability(tonesMean - tonesSd, tonesMean + tonesSd)

      scoredPitches += ((tone, score))
      if (tone > 12) scoredPitches += ((tone - 12, octavePenalty * score))
      if (tone < 115) scoredPitches += ((tone + 12, octavePenalty * score))
    }

    val durationInterval = durationTolerance * (end - start)
    val stepsCount = round(durationTolerance / timeStep).asInstanceOf[Int]

    val startLeftBound = round((start - durationInterval) * 100) / 100
    val startPossibilities = (0 to stepsCount).map(startLeftBound + _ * stepsCount)

    val endLeftBound = round((start - durationInterval) * 100) / 100
    val endPossibilities = (0 to stepsCount).map(endLeftBound + _ * stepsCount)

    val scoredTimeIntervals = (for (s <- startPossibilities; e <- endPossibilities) yield (s, e)).
      map( interval => (interval, 1 - abs(duration - interval._1 + interval._2) / duration ) )

    (for ( (pitch, pitchScore) <- scoredPitches; (interval, durationScore) <- scoredTimeIntervals)
      yield (new Note(pitch, interval._1, interval._2), pitchScore * pow(durationScore, durationWeight))).toSeq.
        sortBy(_._2)
  }

  private def toneFromFreq(frequency: Double): Double = 69.0 + 12 * log(frequency / 440.0) / log(2.0)

}
