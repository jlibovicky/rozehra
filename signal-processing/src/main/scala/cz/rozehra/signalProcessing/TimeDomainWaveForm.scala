package cz.rozehra.signalProcessing

import scalala.library.Plotting
import scalala.tensor.dense._


class TimeDomainWaveForm[T <: Double] (val samplingRate: Frequency , val samples: IndexedSeq[T]) {
  
  def plot: Unit = {
    Plotting.plot(DenseVector.range(0, samples.length) / samplingRate, new DenseVectorRow(samples.toArray[Double]))
    Plotting.title("Wave file")
    Plotting.xlabel("Time in seconds")  
  }
  
  def plot(fileName: String): Unit = {
    plot
    Plotting.saveas(fileName)
  }
}