package cz.rozehra.signalProcessing.trackSelection

import java.text.DecimalFormat
import math._
import cz.rozehra.signalProcessing.languageModeling.LMFormat
import cz.rozehra.signalProcessing.languageModeling.LMFormat.LMFormat

class Hypothesis private (val notes: Seq[Note], val actualScore: Double, val normConstant: Double) {
  val densityBonification = TrackSelectionParameters.densityBonification

  def this(notes: Seq[Note], actualScore: Double) = this(notes, actualScore, 0)
  val scorePerNote = 0 // actualScore / pow(notes.size, densityBonification)
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

      val interval = previousNote.pitch - note.pitch
      builder ++= formatInterval(format, interval)

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

    val prefix = if (secondDuration >= firstDuration) ""
                 else "1/"
    val ratioValue = if (secondDuration >= firstDuration) notNegativeZero(secondDuration / firstDuration)
                     else notNegativeZero(firstDuration / secondDuration)

    if (format == LMFormat.Round0Rat) prefix + noPlace.format(ratioValue)
    else if (format == LMFormat.Round0RatClust) {
      val clustRatio = if (ratioValue <= 8) ratioValue
                       else if (ratioValue <= 16) "9to16"
                       else if (ratioValue <= 32) "17to32"
                       else "over32"
      prefix + clustRatio
    }
    else null
  }

  private def formatInterval(format: LMFormat, interval: Int): String = {
    if (format == LMFormat.Round0RatClust) {
      val absValue = abs(interval)
      val clustValue = if (interval <= 16) absValue.toString
                       else if (interval <= 24) "to2Oct"
                       else if (interval <= 36) "to3Oct"
                       else "over3Oct"
      (if (interval < 0) "-" else "") + clustValue
    }
    interval.toString
  }

  def addNote(note: Note, noteScore: Double): Hypothesis = new Hypothesis(notes :+ note, actualScore + noteScore)

  override def toString = notes.mkString(" ")

  def setNormConstant(normConst: Double) = new Hypothesis(notes, actualScore, normConst)
}
