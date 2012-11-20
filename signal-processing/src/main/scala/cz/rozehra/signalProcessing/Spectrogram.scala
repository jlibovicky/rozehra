package cz.rozehra.signalProcessing

import scalala.tensor.dense.DenseVector
import scalala.tensor.dense._
import scalala.library.Plotting

class Spectrogram[T <: Double](val spectrumRate: Frequency, val spectra: List[Spectrum[T]], 
    val signalWindowSize: Int, val signalWindowShift: Int) {
  
  def generateSignal: TimeDomainWaveForm[T] = {
    val samplingRate = 2 * spectra.head.maxFrequency
    val listOfWindows = spectra.map(spectrum => spectrum.generateWindow.samples)
    val wholeSignal = listOfWindows.foldRight(List[T]())((windowContent, signalStub) => windowContent.toList ++ signalStub)
    new TimeDomainWaveForm(samplingRate, wholeSignal)
  }  
  
  def plot: Unit = {
    val matrix: DenseMatrix[Double] = DenseMatrix.zeros[Double](spectra.size, spectra.head.amplitudes.size) 
           
      
    def createMatrix(colNum: Int, spectra: List[Spectrum[T]]): Unit = {
      spectra match {
        case Nil => Unit
        case head :: tail => {
          for (i <- 0 until head.amplitudes.size) {
            matrix(colNum, i) = head.amplitudes(i)
          }
        createMatrix(colNum + 1, tail)
        }
      }
    }
    createMatrix(0, spectra)
      
    //Plotting.plot(DenseVector.range(0, amplitudes.length) * bandWidth, amplitudes)
    Plotting.image(matrix)
    Plotting.title("Spectrogram")
    Plotting.xlabel("Time in secods")
  }
    
  def plot(fileName: String): Unit = {
    plot          
    Plotting.saveas(fileName)
  }
}