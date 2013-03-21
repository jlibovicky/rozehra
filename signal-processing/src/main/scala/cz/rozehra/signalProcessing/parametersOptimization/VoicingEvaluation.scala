package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing._
import java.io.{FileInputStream, File}
import scala.math._
import voiceDetection.VoiceDetection


object VoicingEvaluation extends OptimizeFrequenciesBase {
  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

    //  list of sets of peaks <|> correct solutions
    var listsOfFundamentals = Seq.empty[(Seq[Boolean], Seq[Double])]
    var samplingRate = 0.0
    for (file <- files) {
      System.err.print("File " + file.getName + " ... ")
      val solution = loadCorrectSolution(file)

      val wave = new WaveFileReader(new FileInputStream(file))
      samplingRate = VoiceDetection.windowRate(wave.samplingRate)

      val voicing = voiceDetection.VoiceDetection.detectVoicing(wave.toTimeDomainWaveForm)
      listsOfFundamentals :+= (voicing, solution)
      System.err.println("loaded")
    }

    val res = listsOfFundamentals.map( e => evaluateVoicingDet(e._1, e._2, samplingRate))
    println()
    println()
    println("accuracy " + res.foldLeft(0.0)(_ + _) / res.size)
  }

  protected def evaluateVoicingDet(voicingList: Seq[Boolean], solution: Seq[Double], samplingRate: Double): Double  = {
    val voicing = voicingList.toIndexedSeq

    var resSum = 0.0
    val res = for ( (frequency, time) <- solution zip (0 to solution.size).map(_ * 0.01) ) {
      val timeIndex = min(floor(time * samplingRate).asInstanceOf[Int], voicing.size - 1)

      if (frequency == 0) print("0")
      else print("1")

      print(",")
      if (voicing(timeIndex)) println("1")
      else println("0")

      if (frequency == 0 && !voicing(timeIndex)) resSum += 1.0
      else if (voicing(timeIndex)) resSum += 1.0
      else resSum += 0.0
    }

    resSum / solution.size
  }
}
