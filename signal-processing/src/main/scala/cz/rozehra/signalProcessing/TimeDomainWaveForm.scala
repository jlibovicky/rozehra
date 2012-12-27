package cz.rozehra.signalProcessing


class TimeDomainWaveForm[T <: Double] (val samplingRate: Frequency , val samples: IndexedSeq[T]) {
  
  def plot: Unit = {
    /*Plotting.plot(DenseVector.range(0, samples.length) / samplingRate, new DenseVectorRow(samples.toArray[Double]))
    Plotting.title("Wave file")
    Plotting.xlabel("Time in seconds")*/
  }

  def segmentToWindows(windowLenght: Int, windowShift: Int): WindowedTimeDomain[T] = {
    def segmentToWindows0(restOfData: IndexedSeq[T], fromPrevious: IndexedSeq[T], accu: List[Window[T]]): List[Window[T]]  = {
      if (fromPrevious.length == 0) {
        val (fakePrevious, rest) = restOfData.splitAt(windowShift)
        segmentToWindows0(rest, fakePrevious, Nil)
      }
      else if (restOfData.length == 0) accu.reverse
      else if (restOfData.length < windowLenght)
        (new Window[T](samplingRate, windowShift, restOfData.toIndexedSeq) :: accu).reverse
      else {
        val (first, second) = restOfData.splitAt(windowLenght - windowShift)
        segmentToWindows0(second,
          first.take(windowShift),
          new Window[T](samplingRate, windowShift,
            (fromPrevious ++ first).toIndexedSeq) :: accu)
      }
    }

    val windows = segmentToWindows0(samples, IndexedSeq.empty[T], Nil)
    new WindowedTimeDomain(samplingRate, windowLenght, windowShift, windows)
  }

  def getEnergy(windowLength: Int, windowShift: Int): TimeDomainWaveForm[Energy] = {
    if (!this.isInstanceOf[TimeDomainWaveForm[Signal]]) { throw new Exception("Energy can be computed only from signal") }

    val firstWindowValue = samples.take(windowLength).fold(0.0)( (energySum, sample) => (energySum + sample * sample))
    val shiftDifferences = samples.grouped(windowShift).map(
      (window) => window.foldLeft(0.0)( (energySum, sample) => (energySum + sample * sample)) ).toIndexedSeq

    val sizeAsShiftNumber = windowLength / windowShift

    var newSamples = IndexedSeq(firstWindowValue)

    for (i <- sizeAsShiftNumber until shiftDifferences.size) {
      newSamples = newSamples :+ newSamples(i - sizeAsShiftNumber) - shiftDifferences(i - sizeAsShiftNumber) + shiftDifferences(i)
    }

    new TimeDomainWaveForm[Energy](samplingRate / windowShift, newSamples.toIndexedSeq)
  }
  
  def plot(fileName: String): Unit = {
    /*plot
    Plotting.saveas(fileName)*/
  }
}