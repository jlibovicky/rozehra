package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.partialtracking.GenericPartialTracking

class parametrizedPartialTracking(
  override val toneTolerance: Double,
  override val maximumWithoutUpdate: Int,
  override val minimumTrackDensity: Double,
  override val minimumTrackDuration: Int) extends GenericPartialTracking[Double]{}
