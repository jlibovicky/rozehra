package cz.rozehra.signalProcessing.tempo

import scala.math._
import cz.rozehra.signalProcessing._
import collection._

object NoteOnsetDetection {
  val timeCorrection = -0.03

  private def getMaximaFromEnergyFlux(fluxSignal: TimeDomainWaveForm[EnergyFlux]): Seq[Time] = {
    var indexes = List.empty[Int]

    // go through the flux and find the indexes of local maxima
    var tmpMaximumIndex = 0
    var tmpMaximumValue = 0.0
    var inPeakArea = false

    for (i <- 0 until fluxSignal.samples.size) {
      if (!inPeakArea && fluxSignal.samples(i) > 0.0) {
        inPeakArea = true
        tmpMaximumValue = fluxSignal.samples(i)
        tmpMaximumIndex = i
      }
      else if (inPeakArea && fluxSignal.samples(i) == 0.0) {
        indexes ::= tmpMaximumIndex
        inPeakArea = false
      }
      else if (inPeakArea && fluxSignal.samples(i) > tmpMaximumValue) {
        tmpMaximumValue = fluxSignal.samples(i)
        tmpMaximumIndex = i
      }
    }

    def indexesListToTimeSeq(list: List[Int], accu: List[Time]): Seq[Time] = {
      list match {
        case Nil => accu.toSeq
        case i :: is => indexesListToTimeSeq(is, (i / fluxSignal.samplingRate) :: accu)
      }
    }

    indexesListToTimeSeq(indexes, Nil)
  }



  def computeNoteOnsetTimes(energyFlux: TimeDomainWaveForm[EnergyFlux]): Seq[Time] = {
    val positiveOnly = energyFlux.samples.map( e => if (e > 0.0) e else 0.0)
    val afterMedianFilter = Filters.medianFilter(positiveOnly, 501)
    val afterSmoothing = Filters.triangularSmoothIterative(afterMedianFilter, 1200)
    val finalMedianFilter = Filters.medianFilter(afterSmoothing, 501)

    getMaximaFromEnergyFlux(new TimeDomainWaveForm[EnergyFlux](energyFlux.samplingRate, finalMedianFilter.toIndexedSeq))
    val times = getMaximaFromEnergyFlux(energyFlux)

    // the smoothing leads to shift to the right => shift to the left by five constant
    times.map(e => e + timeCorrection )
  }

}