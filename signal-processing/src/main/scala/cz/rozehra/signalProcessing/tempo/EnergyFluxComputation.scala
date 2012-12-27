package cz.rozehra.signalProcessing.tempo

import cz.rozehra.signalProcessing._
import math._
import collection.IndexedSeq

object EnergyFluxComputation {
  val bandsInParallel = 8

  val alpha = 2.0
  val beta = 2.0
  val T1: Time = 0.1
  val T2: Time = 0.7
  val energyWindowSize = 64 // this is in fact two time more
  val energyWindowShift = 32
  val bandWidth: Frequency = 5512.5 // splitting into 8 subbands

  def computeEnergyFlux(signal: TimeDomainWaveForm[Signal]): TimeDomainWaveForm[EnergyFlux] = {
    val bandCount = round(signal.samplingRate / bandWidth).asInstanceOf[Int]

    // adds an immutable indexed sequence to already existing mutable indexed sequence
    def sumSeqs(seq1: collection.mutable.IndexedSeq[EnergyFlux], seq2: IndexedSeq[EnergyFlux]) = {
      for (i <- 0 until min(seq1.length, seq2.length)) seq1(i) += seq2(i)
    }
    val resultSignal = collection.mutable.IndexedSeq.fill[Signal](signal.samples.length / energyWindowShift)(0.0.asInstanceOf[EnergyFlux])

    for (i <- 0 until bandCount / bandsInParallel) {
      val indexes = for (j <- 0 until bandsInParallel) yield (i * bandsInParallel + j)

      indexes.par.map(index => {
        //println("Processing band " + index)
        val jThBandSignalSamples = Filters.bandPassFilter(signal.samples,
          signal.samplingRate, index * bandWidth, (index + 1) * bandWidth)
        val jTheBand = new TimeDomainWaveForm[Signal](signal.samplingRate, jThBandSignalSamples)
        getBandEnergyFlux(jTheBand)
      }).seq.foreach(band => sumSeqs(resultSignal, band))
    }

    val positiveOnly = resultSignal.map( e => if (e > 0.0) e else 0.0)
    val afterMedianFilter = Filters.medianFilter(positiveOnly, 501)
    val afterSmoothing = Filters.triangularSmoothIterative(afterMedianFilter, 1200)
    val finalMedianFilter = Filters.medianFilter(afterSmoothing, 501)

    new TimeDomainWaveForm[EnergyFlux](signal.samplingRate / energyWindowShift, finalMedianFilter.toIndexedSeq)
  }

  private def getBandEnergyFlux(band: TimeDomainWaveForm[Signal]): IndexedSeq[EnergyFlux] = {
    val bandEnergy = band.segmentToWindows(64, 32).toEnergy

    val bandEnergyDerivative = new TimeDomainWaveForm[EnergyFlux](bandEnergy.samplingRate,
      Filters.simpleDerivative(bandEnergy.samples, bandEnergy.samplingRate))

    val filteredFlux = Filters.lowPassFilter[EnergyFlux](bandEnergyDerivative.samples,
      bandEnergyDerivative.samplingRate, 10)

    filteredFlux
  }
}