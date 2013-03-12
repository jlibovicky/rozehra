package cz.rozehra.signalProcessing.trackSelection

import scala.math.pow

class Note(val pitch: Int, val start: Double, val end: Double) {
  def duration: Double = end - start
  def shiftStart(newStart: Double) = new Note(pitch, newStart, end)
  def shiftEnd(newEnd: Double) = new Note(pitch, start, newEnd)
  def shortenTo(newEnd: Double) = new Note(pitch, start, newEnd)
  def pitchAsFrequency = 440 * pow(2, (pitch - 69.0) / 12.0)

  override def toString = pitch + "," + start + "," + end
}
