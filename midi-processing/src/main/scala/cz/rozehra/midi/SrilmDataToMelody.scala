package cz.rozehra.midi

import scala.math.pow

object SRILMDataToMelody {
  private val tempo = 500000l
  private val firstNote = new Note(86, 0, 1, tempo)

  def melodyFromSRILMSentence(sentence: String, format: LMFormat.LMFormat): Melody = {
    var notes = Seq(firstNote)

    var previousNote = firstNote
    var lastPauseDuration = 0.0
    for (intervalString <- sentence.split(" ")) {
      if (intervalString.matches("pause.*")) {
        val durationChange = intervalString.split(";")(0).toDouble
        val durationMult = getDurationMult(durationChange, format)
        lastPauseDuration = previousNote.durationInBeats * durationMult
      }
      else {
        val pitchInterval = intervalString.split(";")(0).toInt
        val durationChange = intervalString.split(";")(0).toDouble

        // determine the real ratio between the previous note / pause duration and this note
        val durationMult = getDurationMult(durationChange, format)
        // the the real duration of current note in milliseconds
        val realDuration = if (lastPauseDuration == 0.0) previousNote.durationInBeats * durationMult
                           else lastPauseDuration * durationMult
        val noteStart = previousNote.end + lastPauseDuration

        val newNote = new Note(previousNote.pitch + pitchInterval, noteStart, noteStart + realDuration, tempo)
        notes :+= newNote

        previousNote = newNote
        lastPauseDuration = 0
      }
    }
    new Melody(notes)
  }

  private def getDurationMult(number: Double, format: LMFormat.LMFormat): Double = {
    if (format == LMFormat.Round1Log || format == LMFormat.Round2Log) pow(2, number)
    else number
  }

  def main(args: Array[String]) {
    var count = 0

    for( ln <- io.Source.stdin.getLines ) {
      melodyFromSRILMSentence(ln, LMFormat.Round1Rat).saveAsMidi("melody" + count + ".mid")
      count += 1
    }
  }
}
