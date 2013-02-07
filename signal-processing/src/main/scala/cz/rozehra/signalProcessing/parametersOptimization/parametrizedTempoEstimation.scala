package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.tempo.GenericTempoEstimation

class parametrizedTempoEstimation(
  override val windowSize: Int,
  override val windowShift: Int,
  override val tempoLowerBound: Double,
  override val tempoUpperBound: Double) extends GenericTempoEstimation {

}
