package cz.rozehra.signalProcessing.tempo

import cz.rozehra.signalProcessing._
import scala.math._

object TempoEstimation extends GenericTempoEstimation {
  override val windowSize = 4096
  override val windowShift = 1024
  override val tempoLowerBound = 0.3 // Hz
  override val tempoUpperBound = 4.0 // Hz

  def main(args: Array[String]) = {
    for (arg <- args) {
      val wave = new WaveFileReader(arg)
      val energyFlux = DefaultEnergyFluxComputation.computeEnergyFlux(wave.toTimeDomainWaveForm)
      val tempo = tempoEstimation(energyFlux) * 60
      println(arg + "\t" + tempo)
    }
  }
}


