package cz.rozehra.signalProcessing.fundamentalsDetection

import cz.rozehra.signalProcessing._

trait FundamentalsDetection {
  def detectFundamentals(signal: TimeDomainWaveForm[Signal]): List[Seq[(Frequency, Double)]]
  def spectrogramSamplingRate(signalSamplingRate: Double): Double
}
