package cz.rozehra.signalProcessing.trackSelection

import java.text.DecimalFormat
import math._
import cz.rozehra.signalProcessing.languageModeling.LMFormat
import cz.rozehra.signalProcessing.languageModeling.LMFormat.LMFormat

class Hypothesis private (val notes: Seq[Note], val actualScore: Double, val normConstant: Double) {
  def this(notes: Seq[Note], actualScore: Double) = this(notes, actualScore, 0)
  val scorePerNote = actualScore / notes.size
  val normScore = scorePerNote - normConstant

  def SRILMString(format: LMFormat): String = {
    val builder = new StringBuilder

    var previousNote = notes.head

    for (note <- notes.drop(1)) {
      val pauseDuration = note.start - previousNote.end
      var previousDuration = previousNote.duration

      if (pauseDuration > 0.03) {
        builder ++= "pause;"
        builder ++= formatTime(format, pauseDuration / previousNote.duration)

        builder ++= " "
        previousDuration = pauseDuration
      }

      val modifiedNote = if (pauseDuration > 0 && pauseDuration < 0.03) note.shiftStart(previousNote.end)
                         else note

      builder ++= (previousNote.pitch - note.pitch).toString
      builder ++= ";"

      builder ++= formatTime(format, modifiedNote.duration / previousDuration)

      builder ++= " "
      previousNote = note
    }
    builder.toString.replaceFirst(" $", "")
  }

  private val noPlace = new DecimalFormat("#")
  private val onePlace = new DecimalFormat("#.#")
  private val twoPlaces = new DecimalFormat("#.##")

  private def formatTime(format: LMFormat, number: Double): String = {
    def notNegativeZero(num: Double) = if (num == -0.0) 0.0 else num
    if (format == LMFormat.Round1Rat) {
      onePlace.format(notNegativeZero(number))
    }
    else if (format == LMFormat.Round0Rat) {
      noPlace.format(notNegativeZero(number))
    }
    else if (format == LMFormat.Round2Rat) {
      twoPlaces.format(notNegativeZero(number))
    }
    else if (format == LMFormat.Round1Log) {
      onePlace.format((notNegativeZero(log(number) / log(2))))
    }
    else if (format == LMFormat.Round2Log) {
      twoPlaces.format((notNegativeZero(log(number) / log(2))))
    }
    else null
  }

  def addNote(note: Note, noteScore: Double): Hypothesis = new Hypothesis(notes :+ note, actualScore + noteScore)

  override def toString = notes.mkString(" ")

  def setNormConstant(normConst: Double) = new Hypothesis(notes, actualScore, normConst)
}
