package cz.rozehra.signalProcessing

import org.apache.commons.math3.complex.Complex

class FreqDomainWindow(val withWindowShift: Int, val samplingRate: Frequency, val transform: IndexedSeq[Complex]) {
  def this(window: Window[Signal]) = this(window.withShift, window.samplingRate, null)
}
