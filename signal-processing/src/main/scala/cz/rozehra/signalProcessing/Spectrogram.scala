package cz.rozehra.signalProcessing

import org.apache.commons.math3.linear.{Array2DRowRealMatrix, RealMatrix}
import java.io._
import java.util.Random

class Spectrogram[T <: Double](val spectrumRate: Frequency, val spectra: List[Spectrum[T]],
    val signalWindowSize: Int, val signalWindowShift: Int) {
  def this(matrix: RealMatrix, withWindowShift: Int, bandWidth: Frequency, spectrumRate: Frequency, signalWindowSize: Int) =
    this(spectrumRate, matrixToSpectra[T](matrix, withWindowShift, bandWidth), signalWindowSize, withWindowShift)

  val bandWidth = spectra.head.bandWidth
  val bandsCount = spectra.head.bandsCount
  val spectrumDuration = spectra.head.duration
  val maxFrequency = spectra.head.maxFrequency
  val duration = spectra.size * spectra.head.duration

  def asMatrix: Array2DRowRealMatrix = {
    val matrix = new Array2DRowRealMatrix(bandsCount, spectra.size)
    for (i <- 0 until spectra.size) matrix.setColumn(i, spectra(i).amplitudes.toArray)
    matrix
  }

  def gnuplot(fileName: String): Unit = {
    val dataFileName = fileName + "." + new Random().nextInt() + ".dat"
    val matrix = asMatrix

    val writer = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(dataFileName)))
    for (lineNum <- 0 until matrix.getRowDimension) {
      val row = matrix.getRow(lineNum)
      writer.write(row.mkString(" "))
      writer.newLine()
    }
    writer.close

    val gnuplotProcess = new ProcessBuilder("C:\\Octave\\3.2.4_gcc-4.4.0\\bin\\gnuplot.exe").start()
    val gnuplotWriter = new BufferedWriter(new OutputStreamWriter(gnuplotProcess.getOutputStream))

    gnuplotWriter.write("set term png size 1100,750"); gnuplotWriter.newLine()
    gnuplotWriter.write("set output \"" + fileName + "\""); gnuplotWriter.newLine()
    gnuplotWriter.write("unset key"); gnuplotWriter.newLine()
    gnuplotWriter.write("unset colorbox"); gnuplotWriter.newLine()
    gnuplotWriter.write("set palette gray negative"); gnuplotWriter.newLine()
    gnuplotWriter.write("set xlabel 'Time [s]'"); gnuplotWriter.newLine()
    gnuplotWriter.write("set ylabel 'Frequency [kHz]'"); gnuplotWriter.newLine()
    gnuplotWriter.write("set xrange[0:"+spectra.size+"]"); gnuplotWriter.newLine()
    gnuplotWriter.write("set yrange[0:"+bandsCount+"]"); gnuplotWriter.newLine()
    gnuplotWriter.write("replot"); gnuplotWriter.newLine()
    gnuplotWriter.write("plot '" + dataFileName + "' matrix with image"); gnuplotWriter.newLine()
    gnuplotWriter.flush
    gnuplotWriter.close

    gnuplotProcess.waitFor

    new File(dataFileName).delete
  }
}