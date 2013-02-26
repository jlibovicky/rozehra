package cz.rozehra.signalProcessing


class TimeDomainWaveForm[T <: Double] (val samplingRate: Frequency , val samples: IndexedSeq[T]) {
  
  def plot: Unit = {
    /*Plotting.plot(DenseVector.range(0, samples.length) / samplingRate, new DenseVectorRow(samples.toArray[Double]))
    Plotting.title("Wave file")
    Plotting.xlabel("Time in seconds")*/
  }

  def segmentToWindows(windowLength: Int, windowShift: Int): WindowedTimeDomain[Signal] = {
    val windowsDataMain = samples.sliding(windowLength, windowShift).toSeq

    val restSamplesOnRight = samples.size - (samples.size / windowShift) * windowShift
    val windowsData = if (restSamplesOnRight > 0) windowsDataMain :+ samples.takeRight(restSamplesOnRight)
    else windowsDataMain
    val windows = windowsData.map( s => new Window[Signal](samplingRate, windowShift, s.toIndexedSeq, windowLength))

    new WindowedTimeDomain(samplingRate, windowLength, windowShift, windows.toList)
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