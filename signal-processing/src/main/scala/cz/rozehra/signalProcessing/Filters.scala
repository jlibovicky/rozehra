package cz.rozehra.signalProcessing

import scala.math._

object Filters {
  def lowPassFilter[T <: Double](signal: IndexedSeq[T], samplingRate: Frequency, cutOffFreq: Frequency) = {
    if (cutOffFreq == 0) IndexedSeq.fill[T](signal.size)(0.0.asInstanceOf[T])
    else {
      val dt = 1.0 / samplingRate
      val RC = 1.0 / 2.0 / Pi / cutOffFreq
      val alpha = dt / (RC + dt)

      var resultSignalRev = List(0.0.asInstanceOf[T])
      var prevSignalSample = 0.0

      for (i <- 1 until signal.size) {
        //y[i] := α * x[i] + (1-α) * y[i-1]
        val valueToAdd = (alpha * signal(i) + (1 - alpha) * prevSignalSample).asInstanceOf[T]
        resultSignalRev ::= valueToAdd
        prevSignalSample = valueToAdd
      }

      resultSignalRev.reverse.toIndexedSeq
    }
  }

  def highPassFilter[T <: Double](signal: IndexedSeq[T], samplingRate: Frequency, cutOffFreq: Frequency) = {
    val dt = 1.0 / samplingRate
    val RC = 1.0 / 2.0 / Pi / cutOffFreq
    val alpha = dt / (RC + dt)

    var resultSignalRev = List(0.0.asInstanceOf[T])
    //var previousSample = signal.head
    var previousResultSample = 0.0.asInstanceOf[T]

    for (i <- 1 until signal.size) {
      val newValue = (alpha * (previousResultSample + signal(i) - signal(i - 1))).asInstanceOf[T]
      resultSignalRev ::= newValue
      previousResultSample = newValue
    }

    resultSignalRev.reverse.toIndexedSeq
  }

  def bandPassFilter[T <: Double](signal: IndexedSeq[T], samplingRate: Frequency, from: Frequency, to: Frequency) =
    highPassFilter[T](lowPassFilter[T](signal, samplingRate, from), samplingRate, to)

  def simpleDerivative(signal: IndexedSeq[Double], samplingRate: Frequency) = {
    val resultSignal = collection.mutable.IndexedSeq.fill[Double](signal.size)(0.0)
    val dt = 1 / samplingRate

    resultSignal(0) = signal(0) / dt
    for (i <- 1 until signal.size) {
      resultSignal(i) = (signal(i) - signal(i - 1)) / dt
    }

    resultSignal.toIndexedSeq
  }

  /**
   * Performes the triangular smooth of the given signal. See http://terpconnect.umd.edu/~toh/spectrum/Smoothing.html
   * for details.
   * @param signal
   * @tparam T
   */
  def triangularSmooth[T <: Double](signal: IndexedSeq[T]): IndexedSeq[T] = {
    val resultSignal = collection.mutable.IndexedSeq.fill[T](signal.size)(0.0.asInstanceOf[T])

    resultSignal(0) = signal(0)
    if (signal.size >= 3) resultSignal(1) = ((signal(0) + signal(1) + signal(2)) / 3.0).asInstanceOf[T]

    for (i <- 2 until signal.size - 3) {
      resultSignal(i) =  ((signal(i - 2) + 2 * signal(i - 1) + 3 * signal(i) + 2 * signal(i + 1) + signal(i + 2)) / 9.0).asInstanceOf[T]
    }

    if (signal.size >= 3) resultSignal(1) = ((signal(signal.size - 3) + signal(signal.size - 2) + signal(signal.size - 1)) / 3.0).asInstanceOf[T]
    resultSignal(signal.size - 1) = signal(signal.size - 1)

    resultSignal.toIndexedSeq
  }

  def triangularSmoothIterative[T <: Double](signal: IndexedSeq[T], iterations: Int): IndexedSeq[T] = {
    iterations match {
      case 0 => signal
      case _ => triangularSmoothIterative(triangularSmooth(signal), iterations - 1)
    }
  }

  def medianFilter[T <: Double](signal: IndexedSeq[T], windowSize: Int): IndexedSeq[T] = {
    def median(seq: IndexedSeq[T]) = (seq.sortWith((e1, e2) => e1 < e2))(ceil(seq.size / 2.0).asInstanceOf[Int])

    if (windowSize % 2 == 0) throw new RuntimeException("Window size must be an even number")
    val halfWindow = (windowSize - 1) / 2

    val resultSignal = collection.mutable.IndexedSeq.fill[T](signal.size)(0.0.asInstanceOf[T])

    //resultSignal(1) = if (signal(1) > median(signal.slice(0, 3))) signal(1) else 0.0.asInstanceOf[T]

    for (i <- halfWindow until signal.size - halfWindow) {
      resultSignal(i) = if (signal(i) > median(signal.slice(i - halfWindow, i + halfWindow))) signal(i) else 0.0.asInstanceOf[T]
    }

    //resultSignal(signal.size - 2) = if (signal(1) > median(signal.slice((signal.size - 3), (signal.size))))
    //  signal(signal.size - 2) else 0.0.asInstanceOf[T]

    resultSignal.toIndexedSeq
  }

  def findPeaks[T <: Double](delta: Double, values: IndexedSeq[T]): Set[(Int, Double)] = {
    var peaks = Set.empty[(Int, Double)]

    var minimum = (-1, Double.PositiveInfinity)
    var maximum = (-1, Double.NegativeInfinity)

    var lookForMax = true
    for (i <- 0 until values.size) {
      val value = values(i)

      if (value > maximum._2) maximum = (i, value)
      if (value < minimum._2) minimum = (i, value)

      if (lookForMax && value < maximum._2 - delta) {
        peaks += maximum
        minimum = (i, value)
        lookForMax = false
      }
      else if (value > minimum._2 + delta) {
        maximum = (i, value)
        lookForMax = true
      }
    }
    peaks
  }
}