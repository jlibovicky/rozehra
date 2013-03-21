package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._

object PartialTrackingForCBHSB extends GenericPartialTracking[Signal] {
  override val toneTolerance: Double =  0.7
  override val maximumWithoutUpdate: Int = 2
  override val minimumTrackDensity: Double = 0.6
  override val minimumTrackDuration: Int = 1
}
