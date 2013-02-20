package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._

object PartialTrackingForPeaks  extends GenericPartialTracking[Signal] {
  //0.8	12	0.9	7

  override val toneTolerance: Double = 0.8
  override val maximumWithoutUpdate: Int = 12 //3
  override val minimumTrackDensity: Double = 0.9 //0.7
  override val minimumTrackDuration: Int = 7 //5
}
