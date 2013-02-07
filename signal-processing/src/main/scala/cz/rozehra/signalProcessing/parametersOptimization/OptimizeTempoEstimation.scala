package cz.rozehra.signalProcessing.parametersOptimization

import java.io.{FileInputStream, File}
import scala.io.Source
import cz.rozehra.signalProcessing.{WaveFileReader, TimeDomainWaveForm, Signal}
import cz.rozehra.signalProcessing.tempo.DefaultEnergyFluxComputation

object OptimizeTempoEstimation {
  // energy flux
  val alphas = Seq(1.0, 2.0, 3.0, 4.0)
  val betas = Seq(1.0, 2.0, 3.0, 4.0)
  val T1s = Seq(0.1)
  val T2s = Seq(0.7)
  val energyWindowSizes = Seq(32, 64, 128)
  val bandWidths = Seq(44100.0, 22050.0, 11025.0, 5512.5, 2756.25)

  // tempo estimation
  val tempoLowerBounds = Seq(0.1, 0.2, 0.25, 0.3, 0.35, 0.4)
  val tempoUpperBounds = Seq(4.0, 3.75, 3.5, 3.25, 3.0, 2.75)
  val tempoWindowSizes = Seq(1024, 2048, 4096, 8192)
  val shiftFractionDenominators = Seq(1, 2, 4, 8)


  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

    println("alpha,beta,T1,T2,energyWindowSize,bandwidth,tempoLowerBound,tempoUpperBound," +
      "tempoWindowSize,tempoWindowShift,succesRate,computationTime")

    var dataSet = Seq.empty[(TimeDomainWaveForm[Signal], Double, Double)]
    println("preloading of the files started")
    for (file <- files) {
      val signal = new WaveFileReader(new FileInputStream(file)).toTimeDomainWaveForm
      val tempos = readTemposFromFile(file.getAbsolutePath.replaceFirst("\\.wav", ".txt"))
      dataSet :+= (signal, tempos._1, tempos._2)
      print(".")
    }
    println();

    println("starting the testing phase")
    for (alpha <- alphas; beta <- betas; T1 <- T1s; T2 <- T2s; energyWindowSize <- energyWindowSizes;
         bandWidth <- bandWidths; tempoLowerBound <- tempoLowerBounds; tempoUpperBound <- tempoLowerBounds;
         tempoWindowSize <- tempoWindowSizes; shiftFractionDenominator <- shiftFractionDenominators) {
      val energyFluxEstimator = new parametrizedEnergyFluxComputation(alpha, beta, T1, T2, energyWindowSize,
        energyWindowSize / 2, bandWidth)
      val tempoEstimator = new parametrizedTempoEstimation(tempoWindowSize, tempoWindowSize / shiftFractionDenominator,
        tempoLowerBound, tempoUpperBound)

      val startTime = System.currentTimeMillis()
      val succesRate = evaluateConfiguration(dataSet, energyFluxEstimator, tempoEstimator)
      val duration = (System.currentTimeMillis() - startTime) / 1000

      println(alpha + "," + beta + "," + T1 + "," + T2 + "," + energyWindowSize + "," + bandWidth + "," +
        tempoLowerBound + "," + tempoUpperBound + "," + tempoWindowSize + "," +
        (tempoWindowSize / shiftFractionDenominator) + "," + succesRate + "," + duration);
    }
  }

  def evaluateConfiguration(signals: Seq[(TimeDomainWaveForm[Signal], Double, Double)],
     energyFluxEstimator: parametrizedEnergyFluxComputation, tempoEstimator: parametrizedTempoEstimation): Double = {
    println("testing a set of parameters")
    var succesfulCount = 0
    for ((signal, tempo1, tempo2) <- signals ) {
      val energyFlux = energyFluxEstimator.computeEnergyFlux(signal)
      val tempo = tempoEstimator.tempoEstimation(energyFlux) * 60

      if ((tempo > 0.92 * tempo1 && tempo < 1.08 * tempo1) ||
          (tempo > 0.92 * tempo2 && tempo < 1.08 * tempo2)) succesfulCount += 1
    }
    println("  > file processed")
    (succesfulCount + 0.0) / signals.size
  }

  private def readTemposFromFile(path: String): (Double, Double) = {
    val line = Source.fromFile(path).getLines().take(1).next()
    val tokens = line.split("\t")
    (tokens(0).toDouble, tokens(1).toDouble)
  }
}
