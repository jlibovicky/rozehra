package cz.rozehra.signalProcessing

class Spectrogram[T <: Double](val spectrumRate: Frequency, val spectra: List[Spectrum[T]],
    val signalWindowSize: Int, val signalWindowShift: Int) {

  def bandWidth = spectra.head.bandWidth
  def bandsCount = spectra.head.bandsCount
  def spectrumDuration = spectra.head.duration
  def maxFrequency = spectra.head.maxFrequency

  def plot: Unit = {
    /*val matrix: DenseMatrix[Double] = DenseMatrix.zeros[Double](spectra.head.amplitudes.size, spectra.size)
           
      
    def createMatrix(colNum: Int, spectra: List[Spectrum[T]]): Unit = {
      spectra match {
        case Nil => Unit
        case head :: tail => {
          for (i <- 0 until head.amplitudes.size) {
            matrix(head.amplitudes.length - i - 1, colNum) = head.amplitudes(i)
          }
        createMatrix(colNum + 1, tail)
        }
      }
    }
    createMatrix(0, spectra)
      
    //Plotting.plot(DenseVector.range(0, amplitudes.length) * bandWidth, amplitudes)
    Plotting.image(matrix)
    Plotting.title("Spectrogram")
    Plotting.xlabel("Time in secods") */
  }
    
  def plot(fileName: String): Unit = {
    /*plot
    Plotting.saveas(fileName)*/
  }
}