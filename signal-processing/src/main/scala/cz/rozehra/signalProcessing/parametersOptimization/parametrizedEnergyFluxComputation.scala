package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.tempo.GenericEnergyFluxComputation
import cz.rozehra.signalProcessing.{Frequency, Time}

class parametrizedEnergyFluxComputation(
  override val alpha: Double,
  override val beta: Double,
  override val T1: Time,
  override val T2: Time,
  override val energyWindowSize: Int,
  override val energyWindowShift: Int,
  override val bandWidth: Frequency) extends GenericEnergyFluxComputation{
}
