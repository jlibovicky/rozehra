package cz.rozehra.signalProcessing.fundamentalsDetection.klapuriPerceptional

import scala.math._
import cz.rozehra.signalProcessing._
import org.apache.commons.math3.linear.{BlockFieldMatrix, RealMatrix, Array2DRowRealMatrix}
import org.apache.commons.math3.complex.{ComplexField, Complex}

object PerceptionalFundamentalsDetection {
  val centroidsCount = 30
  val freqLowerBound = 30
  val freqUpperBound = 5200

  private def ERBS(f: Double) = 21.4 * log10(1 + 0.00437 * f)
  private def inverseERBS(k: Double) = (pow(10, k / 21.4) - 1) / 0.00437
  private val firstCentroid = ERBS(freqLowerBound)
  private val centroidStep = (ERBS(freqUpperBound) - firstCentroid) / 72

  val centroids = (0 until centroidsCount).map( i => inverseERBS(firstCentroid + i * centroidStep))
  val bandwidths = (0 until centroidsCount).map(centroids(_) * 0.108 + 24.7)
  val nu = 0.33

  def computePerceptionalSpectrogram(signal: TimeDomainWaveForm[Signal]) = {
    val signalWindowShift = 2048
    val signalWindowSize = 4096
    val spectrumRate = signal.samplingRate / signalWindowShift
    val bandWidth = signal.samplingRate / signalWindowSize
    val windowsCount = ceil(signal.samples.size / signalWindowShift).toInt

    var matrix = new BlockFieldMatrix[Complex](ComplexField.getInstance(),signalWindowSize, windowsCount + 1)

    for (i <- 0 until centroidsCount) {
      val iThBand = Filters.bandPassFilter(signal.samples, signal.samplingRate,
        centroids(i) - bandwidths(i) / 2, centroids(i) + bandwidths(i) / 2)
      val compressedRectified = iThBand.map(x => HWR(FWC(x)))
      val lowPassFiltered = Filters.lowPassFilter(compressedRectified, signal.samplingRate, centroids(i) * 2)
      val z_c = new TimeDomainWaveForm[Signal](signal.samplingRate, lowPassFiltered)
      val Z_c = z_c.segmentToWindows(4096, 2048).windows.map(w => new FreqDomainWindow(w))

      val matrixToAdd = new BlockFieldMatrix[Complex](ComplexField.getInstance(), signalWindowSize, windowsCount + 1)
      for ((window, index) <- Z_c.zipWithIndex)
        matrixToAdd.setColumn(index, window.values.toArray)

      matrix = matrix add matrixToAdd
    }

    var spectraRev = List.empty[Spectrum[Signal]]
    for (i <- 0 until matrix.getColumnDimension) {
      val amplitudes  = matrix.getColumn(i).take(matrix.getRowDimension / 2).map(_.abs)
      spectraRev ::= new Spectrum[Signal](signalWindowShift, bandWidth, amplitudes)
    }

    new Spectrogram[Signal](spectrumRate, spectraRev.reverse, signalWindowSize, signalWindowShift)
  }

  private def FWC(x: Double) = if (x >= 0) pow(x, nu)
                               else -pow(-x, nu)

  private def HWR(x: Double) = max(0, x)

}
