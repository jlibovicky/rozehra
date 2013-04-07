package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._

object PartialTrackingForCombined extends GenericPartialTracking[Signal] {
  override val toneTolerance: Double =  0.6
  override val maximumWithoutUpdate: Int = 12
  override val minimumTrackDensity: Double = 0.2
  override val minimumTrackDuration: Int = 1
}
