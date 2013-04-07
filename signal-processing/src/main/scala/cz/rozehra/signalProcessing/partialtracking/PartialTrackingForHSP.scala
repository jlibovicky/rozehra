package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._

object PartialTrackingForHSP extends GenericPartialTracking[Signal] {
  override val toneTolerance: Double =  0.6
  override val maximumWithoutUpdate: Int = 11
  override val minimumTrackDensity: Double = 0.9
  override val minimumTrackDuration: Int = 6
}
