package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._
import scala.math._

class Track(val start: Int, val end: Int, val frequencies: List[Frequency], val amplitudes: List[Double], val lastFrequencyAddedTime: Int) {
  val averageFrequency = frequencies.foldLeft(0.0)(_ + _) / frequencies.size
  val averageAmplitude = amplitudes.foldLeft(0.0)(_ + _) / amplitudes.size
  val duration = end - start + 1
  val density = frequencies.size.asInstanceOf[Double] / (end - start + 1).asInstanceOf[Double]
  val pitch = 69 + 12 * log(averageFrequency / 440.0) / log(2)

  def addPeak(peak: Double, peakAmplitude: Double, time: Int) = {
    new Track(start, time, peak :: frequencies, peakAmplitude :: amplitudes, time)
  }
  def incEndTime = new Track(start, end + 1, frequencies, amplitudes, lastFrequencyAddedTime)
  def terminate = new Track(start, lastFrequencyAddedTime, frequencies, amplitudes, lastFrequencyAddedTime)

  def merge(t: Track) = new Track(min(start, t.start), max(end, t.end), frequencies ++ t.frequencies,
    amplitudes ++ t.amplitudes, max(lastFrequencyAddedTime, t.lastFrequencyAddedTime))

  override def toString = {
    ("from " + start + " to " + end + " frequency: " + averageFrequency)
  }
}
