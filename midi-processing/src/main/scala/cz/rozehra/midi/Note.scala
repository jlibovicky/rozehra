package cz.rozehra.midi

class Note private (val pitch: Int, val start: Long, val end: Long, tempo: Long) {
  def this(pitch: Int, start: Long, tempo: Long) = this(pitch, start, -1, tempo)
  def finished(timeOfEnd: Long) = new Note(pitch, start, timeOfEnd, tempo)
  def durationInBeats = (end - start).asInstanceOf[Double] / tempo
  def durationInMillis = end - start
  def shortenTo(newEndTime: Long) = new Note(pitch, start, newEndTime, tempo)
}
