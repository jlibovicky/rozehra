package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.pitchDetection.FundamentalsDetection
import cz.rozehra.signalProcessing.partialtracking.PartialTrackingForCBHSP
import cz.rozehra.signalProcessing.pitchDetection.harmonicSpectrumProduct.CBHSP
import java.io.File
import cz.rozehra.signalProcessing.trackSelection.TrackSelectionParameters

object TrackSearchOptimization extends OptimizeTrackSearchBase {
  override val fundamentalsAlgorithm: FundamentalsDetection = CBHSP
  override val partialTrackingAlgorithm = PartialTrackingForCBHSP

  val octavePenalties = Seq(0.01, 0.05, 0.1, 0.5, 1.0)
  //val octavePenalties = Seq(0.1, 0.5, 1.0)
  val durationTolerances = Seq(0.01, 0.02, 0.03, 0.04, 0.05)
  val timeSteps = Seq(0.01, 0.02, 0.03, 0.04, 0.05)
  val durationWeights = Seq(0.1, 0.5, 1.0, 2.0, 3.0, 4.0) // it's an exponent
  val densityBonifications = Seq(0.5, 1.0, 2.0, 3.0, 4.0, 5.0)
  val nBestSizes = Seq(10, 20, 50)

  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))
    val startPos = args(1).toInt
    val endPos = args(2).toInt

    println("octavePenalty,durationTolerance,timeStep,durationWeight,densityBonification,nBestSize,voicingAcc," +
      "pitchAccuracy,toneAccuracy,overallAccuracy")
    val allFilesTracks = files.map(f => getPartialTracks(f.getAbsolutePath))
    var exploredOptions = 0
    for (octavePenalty <- octavePenalties;
         durationTolerance <- durationTolerances;
         timeStep <- timeSteps;
         durationWeight <- durationWeights;
         densityBonification <- densityBonifications;
         nBestSize <- nBestSizes) {
      exploredOptions += 1
      if (exploredOptions >= endPos) sys.exit(0)
      else if (exploredOptions >= startPos) {
        TrackSelectionParameters.octavePenalty = octavePenalty
        TrackSelectionParameters.durationTolerance = durationTolerance
        TrackSelectionParameters.timeStep = timeStep
        TrackSelectionParameters.durationWeight = durationWeight
        TrackSelectionParameters.nBestSize = nBestSize

        val scores = allFilesTracks.map(t => testJustTracks(t._1, t._2, t._3))
        val scoreSums =
          scores.foldLeft((0.0, 0.0, 0.0, 0.0))( (s, e) => (s._1 + e._1, s._2 + e._2, s._3 + e._3, s._4 + e._4 ))

        val voicingAcc = scoreSums._1 / files.size
        val pitchAccuracy = scoreSums._2 / files.size
        val toneAccuracy = scoreSums._3 / files.size
        val overallAccuracy = scoreSums._4 / files.size

        println(Seq(octavePenalty, durationTolerance, timeStep, durationWeight, densityBonification, nBestSize,
          voicingAcc, pitchAccuracy, toneAccuracy, overallAccuracy).mkString(","))
      } else Unit
    }

  }
}
