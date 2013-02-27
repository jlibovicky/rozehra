package cz.rozehra.signalProcessing

import scala.collection.immutable.IndexedSeq
import scala.math._
import fft.{FFT}

class Window[T <: Double](val samplingRate: Frequency, val withShift: Int,
                          val samples: IndexedSeq[T], val maxFrameSize: Int) {
    def size = samples.length

    def +(that: Window[T]) = 
      if (samplingRate != that.samplingRate || size != that.size) 
        throw new Exception("Windows must have the same lenght and sampling rate to be added");       
      else 
        sumSequences[T](samples, that.samples)
        
    def toSpectrum: Spectrum[T] = {
      val spectrum = FFT.powerSpectrum(FFT.hanningWindow(samples), maxFrameSize)
      val bandWidth: Frequency = samplingRate / 2 / spectrum.length 
      new Spectrum[T](withShift, bandWidth, spectrum)
    }

    def toZeroPaddedSpectrum: Spectrum[T] = {
      val spectrum = FFT.powerSpectrum(FFT.hanningWindow(samples), maxFrameSize * 2)
      val bandWidth: Frequency = samplingRate / spectrum.length
      new Spectrum[T](2 * withShift, bandWidth, spectrum)
    }

    def hanningWindow: Window[T] = {
      val newValues = FFT.hanningWindow(samples)
      new Window[T](samplingRate, withShift, newValues, maxFrameSize)
    }

    def changeMaxFrameSize(newMaxFrameSize: Int) =
      new Window[T](samplingRate, withShift, samples, newMaxFrameSize)


    def getEnergy: Energy = {
      if (!this.isInstanceOf[Window[Signal]]) { throw new Exception("Energy can be computed only from signal") }
      samples.foldLeft(0.0)( (sum, sample) => sum + sample * sample )
    }
      
    
    def plot: Unit = {
      /*Plotting.plot(DenseVector.range(0, size) / samplingRate, new DenseVectorRow(samples.toArray[Double]))
      Plotting.title("A window of " + magnitute)
      Plotting.xlabel("Time in seconds")
      Plotting.ylabel(magnitute)*/
    }
        
    def plot(fileName: String): Unit = {
      /*plot
      Plotting.saveas(fileName)*/
    }
}