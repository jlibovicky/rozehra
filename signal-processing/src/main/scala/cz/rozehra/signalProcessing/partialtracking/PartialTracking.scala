package cz.rozehra.signalProcessing

import partialtracking.{GenericPartialTracking, Track}
import scala._
import scala.math._


object PartialTracking extends GenericPartialTracking[Signal] {
  override val medianMultiply: Double = 100.0

  override val toneTolerance: Double = 0.6
  override val maximumWithoutUpdate: Int = 3
  override val minimumTrackDensity: Double = 0.7
  override val minimumTrackDuration: Int = 5
}
