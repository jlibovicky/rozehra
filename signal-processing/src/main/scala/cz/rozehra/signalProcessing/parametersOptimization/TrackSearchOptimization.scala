package cz.rozehra.signalProcessing.parametersOptimization

import cz.rozehra.signalProcessing.fundamentalsDetection.FundamentalsDetection
import cz.rozehra.signalProcessing.partialtracking.PartialTrackingForCBHSB
import cz.rozehra.signalProcessing.fundamentalsDetection.harmonicSpectrumProduct.CBHSP
import java.io.File

object TrackSearchOptimization extends OptimizeTrackSearchBase {
  val fundamentalsAlgorithm: FundamentalsDetection = CBHSP
  override val partialTrackingAlgorithm = PartialTrackingForCBHSB

  def main(args: Array[String]) {
    val testDirectory = new File(args(0))
    val files = testDirectory.listFiles().filter( _.getName.endsWith(".wav"))

    //val allFilesTracks = getPartialTracks()

  }
}
