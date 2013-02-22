package cz.rozehra.midi

import scala.math._
import java.text.DecimalFormat

class Melody (val notes: Array[Note]) extends Serializable {
  def this(notes: Seq[Note]) = this(notes.toArray)
  def durationInBeats = notes.last.end - notes.head.start

  def saveAsMidi(path: String) {
    val midi = new MidiWriter

    var previousNote = notes.head

    midi.noteOnOffNow((previousNote.durationInBeats * 16).toInt,
      previousNote.pitch, 127)
    for (note <- notes.drop(1)) {
      var restTime = 0
      if (previousNote.end < note.start) restTime = (16 * (note.start - previousNote.end)).toInt

      midi.noteOn(restTime, note.pitch, 127)
      midi.noteOff((16 * note.durationInBeats).toInt, note.pitch)

      previousNote = note
    }

    midi.writeToFile(path)
  }

  import LMFormat._

  private val noPlace = new DecimalFormat("#")
  private val onePlace = new DecimalFormat("#.#")
  private val twoPlaces = new DecimalFormat("#.##")

  private def formatTime(format: LMFormat, firstDuration: Double, secondDuration: Double): String = {
    def notNegativeZero(num: Double) = if (num == -0.0) 0.0 else num
    if (format == Round1Rat) {
      if (secondDuration >= firstDuration) onePlace.format(notNegativeZero(secondDuration / firstDuration))
      else "1/" + onePlace.format(notNegativeZero(firstDuration / secondDuration))
    }
    else if (format == Round0Rat) {
      if (secondDuration >= firstDuration) noPlace.format(notNegativeZero(secondDuration / firstDuration))
      else "1/" + noPlace.format(notNegativeZero(firstDuration / secondDuration))
    }
    else if (format == Round2Rat) {
      if (secondDuration >= firstDuration) twoPlaces.format(notNegativeZero(secondDuration / firstDuration))
      else "1/" + twoPlaces.format(notNegativeZero(firstDuration / secondDuration))
    }
    else if (format == RoundHalfRat) {
      if (secondDuration >= firstDuration) {
        val ratio = round(2 * secondDuration / firstDuration) / 2
        onePlace.format(ratio)
      }
      else {
        val ratio = round(2 * firstDuration / secondDuration) / 2
        "1/" + onePlace.format(ratio)
      }
    }
    else if (format == Round1Log) {
      onePlace.format((notNegativeZero(log(secondDuration / firstDuration) / log(2))))
    }
    else if (format == Round2Log) {
      twoPlaces.format((notNegativeZero(log(firstDuration) / log(2))))
    }
    else null
  }

  def getLMNotation(format: LMFormat): String = {
    val builder = new StringBuilder

    var previousNote = notes.head

    for (note <- notes.drop(1)) {
      val pauseDuration = previousNote.tempo * (note.start - previousNote.end) / 1000
      var previousDuration = previousNote.durationInMillis

      if (pauseDuration > 30) {
        builder ++= "pause;"
        builder ++= formatTime(format, pauseDuration, previousNote.durationInMillis)

        builder ++= " "
        previousDuration = pauseDuration
      }

      val modifiedNote = if (pauseDuration > 0 && pauseDuration < 30) note.shiftStart(previousNote.end)
                         else note

      builder ++= (previousNote.pitch - note.pitch).toString
      builder ++= ";"

      builder ++= formatTime(format, modifiedNote.durationInMillis, previousDuration)

      builder ++= " "
      previousNote = note
    }
    builder ++= "\n"
    builder.toString()
  }

  def getMelodyLength: Double = {
    val notesLength = notes.foldLeft(0.0)( _ + _.durationInMillis )
    val spacesLength = (notes.take(notes.size - 1) zip notes.drop(1)).map(
      s => (s._1.tempo + s._2.tempo) / 2 * (s._2.start - s._1.end) / 1000.0).foldLeft(0.0)(_ + _)
    spacesLength + notesLength
  }
}
