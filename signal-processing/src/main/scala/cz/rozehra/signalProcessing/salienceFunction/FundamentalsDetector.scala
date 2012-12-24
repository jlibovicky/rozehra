package cz.rozehra.signalProcessing.salienceFunction

import scala.math._
import cz.rozehra.signalProcessing._

class FundamentalsDetector(val spectrum: Spectrum[Signal], val precomputer: FundamentalsPrecomputer) {
  val alpha = precomputer.alpha
  val beta = precomputer.beta
  val periodStep = precomputer.periodStep
  val harmonicsCount = precomputer.harmonicsCount



  def salienceFunction(period: Time) = {
    (1 to harmonicsCount ).foldLeft(0.0)( (sum, m) =>
      sum + precomputer.gFunction(period, m)
    )

  }
}
