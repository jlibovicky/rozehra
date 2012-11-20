package cz.rozehra.signalProcessing

import cz.rozehra.signalProcessing.Frequency
import scala.math._

object Filters {
  def lowPassFilter[T <: Double](signal: IndexedSeq[T], samplingRate: Frequency, cutOffFreq: Frequency) = {
    val dt = 1 / samplingRate
    val RC = 1 / 2 / Pi / cutOffFreq
    val alpha = dt / (RC + dt)

    val resultSignal = collection.mutable.IndexedSeq.fill[T](signal.size)(0.0.asInstanceOf[T])

    resultSignal(0) = (alpha * signal(0)).asInstanceOf[T]
    for (i <- 1 until signal.size) {
      resultSignal(i) = (resultSignal(i - 1) + alpha * (signal(i) - resultSignal(i - 1))).asInstanceOf[T]
    }

    resultSignal.toIndexedSeq[T]
  }

  def highPassFilter[T <: Double](signal: IndexedSeq[T], samplingRate: Frequency, cutOffFreq: Frequency) = {
    val dt = 1 / samplingRate
    val RC = 1 / 2 / Pi / cutOffFreq
    val alpha = dt / (RC + dt)

    val resultSignal = collection.mutable.IndexedSeq.fill[T](signal.size)(0.0.asInstanceOf[T])

    resultSignal(0) = (alpha * signal(0)).asInstanceOf[T]
    for (i <- 1 until signal.size) {
      resultSignal(i) = (alpha * (resultSignal(i - 1) + signal(i) - signal(i - 1))).asInstanceOf[T]
    }

    resultSignal.toIndexedSeq[T]
  }

  def bandPassFilter[T <: Double](signal: IndexedSeq[T], samplingRate: Frequency, from: Frequency, to: Frequency) =
    highPassFilter[T](lowPassFilter[T](signal, samplingRate, from), samplingRate, to)
}