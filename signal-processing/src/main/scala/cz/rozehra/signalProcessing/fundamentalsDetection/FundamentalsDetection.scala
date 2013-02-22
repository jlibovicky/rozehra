package cz.rozehra.signalProcessing.fundamentalsDetection

import cz.rozehra.signalProcessing._

trait FundamentalsDetection {
  def detectFundamentals(spectrogram: Spectrogram[Signal], originalSamplingRate: Double): List[Seq[(Frequency, Double)]]
}
