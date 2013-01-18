package cz.rozehra.midi


class Melody(val notes: Seq[Note]) {
  def duration = notes.last.end - notes.head.start
}
