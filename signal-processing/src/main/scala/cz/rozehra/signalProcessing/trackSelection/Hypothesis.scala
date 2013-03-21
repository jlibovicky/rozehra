package cz.rozehra.signalProcessing.trackSelection

import java.text.DecimalFormat
import math._
import cz.rozehra.signalProcessing.languageModeling.LMFormat
import cz.rozehra.signalProcessing.languageModeling.LMFormat.LMFormat

class Hypothesis private (val notes: Seq[Note], val actualScore: Double, val normConstant: Double) {
  val densityBonification = TrackSelectionParameters.densitiyBonification

  def this(notes: Seq[Note], actualScore: Double) = this(notes, actualScore, 0)
  val scorePerNote = actualScore / pow(notes.size, densityBonification)
  val normScore = scorePerNote - normConstant

  def SRILMString(format: LMFormat): String = {
    val builder = new StringBuilder

    var previousNote = notes.head

    for (note <- notes.drop(1)) {
      val pauseDuration = note.start - previousNote.end
      var previousDuration = previousNote.duration

      if (pauseDuration >= 0.03) {
        builder ++= "pause;"
        builder ++= formatTime(format, previousNote.duration, pauseDuration)

        builder ++= " "
        previousDuration = pauseDuration
      }

      val modifiedNote = if (pauseDuration > 0 && pauseDuration < 0.03) note.shiftStart(previousNote.end)
                         else note

      builder ++= (previousNote.pitch - note.pitch).toString
      builder ++= ";"

      builder ++= formatTime(format, previousDuration, modifiedNote.duration)

      builder ++= " "
      previousNote = note
    }
    builder.toString.replaceFirst(" $", "")
  }

  private val noPlace = new DecimalFormat("#")

  private def formatTime(format: LMFormat, firstDuration: Double, secondDuration: Double): String = {
    def notNegativeZero(num: Double) = if (num == -0.0) 0.0 else num
    if (format == LMFormat.Round0Rat) {
      if (secondDuration >= firstDuration) noPlace.format(notNegativeZero(secondDuration / firstDuration))
      else "1/" + noPlace.format(notNegativeZero(firstDuration / secondDuration))

    }
    else null
  }

  def addNote(note: Note, noteScore: Double): Hypothesis = new Hypothesis(notes :+ note, actualScore + noteScore)

  override def toString = notes.mkString(" ")

  def setNormConstant(normConst: Double) = new Hypothesis(notes, actualScore, normConst)
}
