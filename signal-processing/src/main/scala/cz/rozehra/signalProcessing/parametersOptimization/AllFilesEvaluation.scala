package cz.rozehra.signalProcessing.parametersOptimization

import java.io.File
import scala.Predef._
import cz.rozehra.signalProcessing.pitchDetection._
import cz.rozehra.signalProcessing.pitchDetection.klapuriWhitening.{KlapuriFundamentalDetection, Whitening}
import cz.rozehra.signalProcessing.pitchDetection.harmonicSpectrumProduct.{CBHSP, HSP}
import cz.rozehra.signalProcessing.partialtracking._
import java.text.DecimalFormat
import cz.rozehra.signalProcessing.PartialTrackingForFundamentals

object AllFilesEvaluation extends OptimizeTrackSearchBase {
  override val fundamentalsAlgorithm = HSP
  override val partialTrackingAlgorithm = PartialTrackingForHSP

  val df: DecimalFormat = new DecimalFormat("0.000")

  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

    var totalVoicingAcc = 0.0
    var totalPitchAcc = 0.0
    var totalChromaAcc = 0.0
    var totalOverallAcc = 0.0

    println("file,voicing accuracy,raw pitch accuracy,raw chroma accuracy,overall accuracy")
    for (file <- files) {
      val score = testAFile(file.getAbsolutePath)
      println(List(file.getName, df.format(score._1), df.format(score._2), df.format(score._3), df.format(score._4)).mkString(","))

      totalVoicingAcc += score._1
      totalPitchAcc += score._2
      totalChromaAcc += score._3
      totalOverallAcc += score._4
    }

    println(List("average", df.format(totalVoicingAcc / files.size), df.format(totalPitchAcc / files.size),
      df.format(totalChromaAcc / files.size), df.format(totalOverallAcc / files.size)).mkString(","))
  }
}
