package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.partialtracking.Track
import math._

trait OptimizePartialTrackingBase extends OptimizeFrequenciesBase {
  protected def evaluateTracks(tracks: Set[Track], solution: Seq[Double], samplingRate: Double):
  (Double, Double, Double, Double) = {
    var containsCorrectTrack = 0.0
    var containsCorrectOctave = 0.0

    var totalFrequencyRecall = 0.0
    var totalToneRecall = 0.0

    for ( (frequency, time) <- solution zip (0 to solution.size).map(_ * 0.01) ) {
      val timeIndex = time * samplingRate
      val trackOnTime = tracks.filter( t => t.start <= timeIndex && t.end >= timeIndex)

      if ((frequency == 0) || // silent place
        (trackOnTime.exists( t => abs(toneFromFreq(t.averageFrequency) - toneFromFreq(frequency)) <= 0.5 )))
        containsCorrectTrack += 1.0

      if ((frequency == 0) || // silent place
        (trackOnTime.exists( t => abs(toneFromFreq(t.averageFrequency) - toneFromFreq(frequency)) % 12.0 <= 0.5 )))
        containsCorrectOctave += 1.0

      if ((frequency == 0) && trackOnTime.isEmpty) { totalFrequencyRecall += 1.0; totalToneRecall += 1.0 }
      else if (!trackOnTime.isEmpty) {
        totalFrequencyRecall += trackOnTime.filter(
          t => abs(toneFromFreq(t.averageFrequency) - toneFromFreq(frequency)) <= 0.5 ).size /
          trackOnTime.size.asInstanceOf[Double]
        totalToneRecall += trackOnTime.filter(
          t => abs(toneFromFreq(t.averageFrequency) - toneFromFreq(frequency)) % 12.0  <= 0.5).size /
          trackOnTime.size.asInstanceOf[Double]
      }

    }
    (containsCorrectTrack / solution.size, containsCorrectOctave / solution.size,
      totalFrequencyRecall / solution.size, totalToneRecall / solution.size)
  }
}
