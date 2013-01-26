package cz.rozehra.midi

/**
 * Represents a note
 * @param pitch Pitch of the note in midi scale (0 - 127)
 * @param start Time of the  start of the note as a number of quarter notes from the beginning
 * @param end Time of the end of the note as a number of quarter notes from the beginning
 * @param tempo Tempo in microseconds per quarter note
 */
class Note (val pitch: Int, val start: Double, val end: Double, val tempo: Long) extends Serializable {
  def this(pitch: Int, start: Double, tempo: Long) = this(pitch, start, -1, tempo)
  def finished(timeOfEnd: Double) = new Note(pitch, start, timeOfEnd, tempo)
  def durationInBeats = end - start
  def durationInMillis = tempo * durationInBeats / 1000.0
  def shortenTo(newEndTime: Double) = new Note(pitch, start, newEndTime, tempo)
  def shiftStart(newStart: Double) = new Note(pitch, newStart, end, tempo)
}
