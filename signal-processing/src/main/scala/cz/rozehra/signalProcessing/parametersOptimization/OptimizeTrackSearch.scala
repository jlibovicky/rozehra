package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.{PartialTrackingForFundamentals, WaveFileReader}
import cz.rozehra.signalProcessing.trackSelection.{Hypothesis, TrackSelection}
import java.io.File
import io.Source
import math._
import scala.Predef._
import cz.rozehra.signalProcessing.fundamentalsDetection.klapuriWhitening.{KlapuriFundamentalDetection, Whitening}
import cz.rozehra.signalProcessing.fundamentalsDetection.harmonicSpectrumProduct.HSP

object OptimizeTrackSearch extends OptimizeTrackSearchBase {
  override val fundamentalsAlgorithm = HSP

  def main(args: Array[String]) {
      val testDirectory = new File(args(0))
      val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

      println("file,voicing accuracy,raw pitch accuracy,raw chroma accuracy,overall accuracy")
      for (file <- files) {
        val score = testAFile(file.getAbsolutePath)
        println(List(file.getName, score._1, score._2, score._3, score._4).mkString(","))
      }
    }
}
