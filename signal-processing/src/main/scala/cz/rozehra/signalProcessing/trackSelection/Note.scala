package cz.rozehra.signalProcessing.trackSelection

class Note(val pitch: Int, val start: Double, val end: Double) {
  def duration: Double = end - start
  def shiftStart(newStart: Double) = new Note(pitch, newStart, end)
  def shortenTo(newEnd: Double) = new Note(pitch, start, newEnd)

  override def toString = pitch + "," + start + "," + end
}
