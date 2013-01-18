package cz.rozehra.signalProcessing

class Spectrogram[T <: Double](val spectrumRate: Frequency, val spectra: List[Spectrum[T]],
    val signalWindowSize: Int, val signalWindowShift: Int) {

  val bandWidth = spectra.head.bandWidth
  val bandsCount = spectra.head.amplitudes.size
  val spectrumDuration = spectra.head.duration
  val maxFrequency = spectra.head.maxFrequency

/*  def generateSignal: TimeDomainWaveForm[T] = {
    val samplingRate = 2 * spectra.head.maxFrequency
    val listOfWindows = spectra.map(spectrum => spectrum.generateWindow.samples)
    val wholeSignal = listOfWindows.foldRight(IndexedSeq[T]())((windowContent, signalStub) => windowContent ++ signalStub)
    new TimeDomainWaveForm(samplingRate, wholeSignal)
  }*/
  
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