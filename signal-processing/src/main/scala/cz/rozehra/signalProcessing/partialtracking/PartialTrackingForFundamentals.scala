package cz.rozehra.signalProcessing

import partialtracking.{GenericPartialTracking, Track}
import scala._
import scala.math._


object PartialTrackingForFundamentals extends GenericPartialTracking[Signal] {
  override val toneTolerance: Double = 0.2
  override val maximumWithoutUpdate: Int = 2 //3
  override val minimumTrackDensity: Double = 0.5 //0.7
  override val minimumTrackDuration: Int = 2 //5
}
