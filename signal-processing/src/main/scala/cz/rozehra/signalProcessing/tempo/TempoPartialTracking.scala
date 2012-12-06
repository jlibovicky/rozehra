package cz.rozehra.signalProcessing.tempo

import cz.rozehra.signalProcessing._;
import cz.rozehra.signalProcessing.partialtracking.GenericPartialTracking
import math._

object TempoPartialTracking extends GenericPartialTracking[EnergyFlux] {
  override val medianMultiply: Double = 500.0

  override val toneTolerance: Double = 0.85
  override val maximumWithoutUpdate: Int = 3
  override val minimumTrackDensity: Double = 0.7
  override val minimumTrackDuration: Int = 1

  override  def appendCondition(avgFreq: Double, peak: Double): Boolean = {
    abs(peak / avgFreq - 1) < toneTolerance
  }
}
