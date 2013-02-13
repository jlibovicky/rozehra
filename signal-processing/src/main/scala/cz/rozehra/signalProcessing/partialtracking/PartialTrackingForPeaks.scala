package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._

object PartialTrackingForPeaks  extends GenericPartialTracking[Signal] {
  override val toneTolerance: Double = 0.6
  override val maximumWithoutUpdate: Int = 4 //3
  override val minimumTrackDensity: Double = 0.4 //0.7
  override val minimumTrackDuration: Int = 3 //5
}
