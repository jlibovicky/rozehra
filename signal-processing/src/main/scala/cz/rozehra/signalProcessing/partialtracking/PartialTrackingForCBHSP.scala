package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._

object PartialTrackingForCBHSP extends GenericPartialTracking[Signal] {
  override val toneTolerance: Double =  0.7
  override val maximumWithoutUpdate: Int = 3
  override val minimumTrackDensity: Double = 0.7
  override val minimumTrackDuration: Int = 1
}
