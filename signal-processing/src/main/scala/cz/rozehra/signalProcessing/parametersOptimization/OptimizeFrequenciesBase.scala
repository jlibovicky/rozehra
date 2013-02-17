package cz.rozehra.signalProcessing.parametersOptimization

import math._
import java.io.File
import io.Source

trait OptimizeFrequenciesBase {
  protected def pitchToFrequency(pitch: Int): Double =  pow(2, (pitch-69.0)/12) * 440

  protected def toneFromFreq(frequency: Double): Double = 69.0 + 12 * log(frequency / 440.0) / log(2.0)

  /**
   * Loads the file with correct solution, assuming that for each wav file there exist a REF.txt
   * @param waveFile A wav file the solution should be load for
   * @return Sequence of correct frequencies by hundredths of seconds
   */
  protected def loadCorrectSolution(waveFile: File): Seq[Double] = {
    val solutionFile = Source.fromFile(waveFile.getAbsolutePath.replaceFirst("\\.wav", "REF.txt"))
    var frequencies = Seq.empty[Double]
    for (line <- solutionFile.getLines()) {
      val frequency: String = line.split('\t')(1)
      frequencies :+= frequency.toDouble
    }
    frequencies
  }
}
