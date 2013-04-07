package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.{PartialTrackingForFundamentals, WaveFileReader}
import cz.rozehra.signalProcessing.pitchDetection.klapuriWhitening.{KlapuriFundamentalDetection, Whitening}
import cz.rozehra.signalProcessing.trackSelection.{Hypothesis, TrackSelection}
import java.io.File
import math._
import cz.rozehra.signalProcessing.pitchDetection.harmonicSpectrumProduct.{CBHSPwithWhitening, CBHSP, HSP}
import cz.rozehra.signalProcessing.pitchDetection.CombinedFundamentalsDetection
import cz.rozehra.signalProcessing.partialtracking.{PartialTrackingForCBHSP, GenericPartialTracking}

object MeasureTrackSearch extends OptimizeTrackSearchBase {
  override val fundamentalsAlgorithm = CBHSP
  override val partialTrackingAlgorithm = PartialTrackingForCBHSP

  def main(args: Array[String]) {
    val score = testAFile("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01.wav")

    println()
    println("Voicing Accuracy: " + score._1)
    println("Raw Pitch Accuracy: " + score._2)
    println("Raw Chroma Accuracy: " + score._3)
    println("Overall Accuracy: " + score._4)
  }


}
