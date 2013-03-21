package cz.rozehra.signalProcessing.parametersOptimization

import java.io.{FileInputStream, File}
import cz.rozehra.signalProcessing._
import fundamentalsDetection.FundamentalsDetection
import fundamentalsDetection.harmonicSpectrumProduct.CBHSP
import fundamentalsDetection.klapuriWhitening.{KlapuriFundamentalDetection, Whitening}
import math._

object VoicingOptimization extends OptimizeFrequenciesBase {
  val fundDetection: FundamentalsDetection = CBHSP

  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

    //  list of sets of peaks <|> correct solutions
    var listsOfFundamentals = Seq.empty[(List[Seq[(Frequency, Double)]], Seq[Double])]
    var samplingRate = 0.0
    for (file <- files) {
      System.err.print("File " + file.getName + " ... ")
      val solution = loadCorrectSolution(file)

      val wave = new WaveFileReader(new FileInputStream(file))
      //val spectrogram = wave.segmentToWindows(4096, 2048).toZeroPaddedSpectrogram
      samplingRate = fundDetection.spectrogramSamplingRate(wave.samplingRate)

      val detectedFundamentals = fundDetection.detectFundamentals(wave.toTimeDomainWaveForm)
      listsOfFundamentals :+= (detectedFundamentals, solution)
      System.err.println("loaded")
    }

    listsOfFundamentals.map( e => evaluateSalienceFunction(e._1, e._2, samplingRate))
  }

  protected def evaluateSalienceFunction(fundamentals: List[Seq[(Frequency, Double)]],
                                         solution: Seq[Double], samplingRate: Double): Unit  = {
    val funds: IndexedSeq[Seq[(Frequency, Double)]] = fundamentals.toIndexedSeq

    val highestFunds = funds.map(_.maxBy(_._2))
    val averageStrength = highestFunds.foldLeft(0.0)( (s, f) => s + f._2) / funds.size

    for ( (frequency, time) <- solution zip (0 to solution.size).map(_ * 0.01) ) {
      val timeIndex = min(floor(time * samplingRate).asInstanceOf[Int], funds.size - 1)

      if (abs(toneFromFreq(highestFunds(timeIndex)._1) - toneFromFreq(frequency)) < 0.25)
        print("1")
      else if (abs(toneFromFreq(highestFunds(timeIndex)._1) % 12 - toneFromFreq(frequency) % 12) < 0.25)
        print("2")
      else print("0")

      print(",")
      print(highestFunds(timeIndex)._2)
      print(",")
      println(highestFunds(timeIndex)._2 / averageStrength)
    }
  }
}
