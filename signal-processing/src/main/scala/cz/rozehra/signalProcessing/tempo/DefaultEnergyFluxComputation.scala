package cz.rozehra.signalProcessing.tempo

import cz.rozehra.signalProcessing._
import math._
import collection.IndexedSeq

object DefaultEnergyFluxComputation extends GenericEnergyFluxComputation {
  val alpha = 2.0
  val beta = 2.0
  val T1: Time = 0.1
  val T2: Time = 0.7
  val energyWindowSize = 64 // this is in fact two time more
  val energyWindowShift = 32
  val bandWidth: Frequency = 5512.5 // splitting into 8 subbands
}