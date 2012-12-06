package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._

class Track(val start: Int, val end: Int, val frequencies: List[Frequency], val amplitudes: List[Double], val lastFrequencyAddedTime: Int) {
  val averageFrequency = frequencies.foldLeft(0.0)(_ + _) / frequencies.size
  val averageAmplitude = amplitudes.foldLeft(0.0)(_ + _) / amplitudes.size
  val duration = end - start + 1
  val density = frequencies.size.asInstanceOf[Double] / (end - start + 1).asInstanceOf[Double]

  def addPeak(peak: Double, peakAmplitude: Double, time: Int) = {
    new Track(start, time, peak :: frequencies, peakAmplitude :: amplitudes, time)
  }
  def incEndTime = new Track(start, end + 1, frequencies, amplitudes, lastFrequencyAddedTime)
  def terminate = new Track(start, lastFrequencyAddedTime, frequencies, amplitudes, lastFrequencyAddedTime)

  override def toString = {
    ("from " + start + " to " + end + " frequency: " + averageFrequency)
  }
}
