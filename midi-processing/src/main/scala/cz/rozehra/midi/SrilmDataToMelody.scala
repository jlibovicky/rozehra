package cz.rozehra.midi

import scala.math.{pow, min, max}

object SRILMDataToMelody {
  private val tempo = 500000l
  private val firstNote = new Note(64, 0, 1, tempo)

  def melodyFromSRILMSentence(sentence: String, format: LMFormat.LMFormat): Melody = {
    var notes = Seq(firstNote)

    var previousNote = firstNote
    var lastPauseDuration = 0.0
    for (intervalString <- sentence.split(" ")) {
      if (intervalString.startsWith("pause")) {
        val durationChangeString = intervalString.split(";")(1)
        val durationChange = if (durationChangeString.matches("^1/.*")) { 1 / durationChangeString.split("/")(1).toDouble }
                             else durationChangeString.toDouble

        val durationMult = getDurationMult(durationChange, format)
        lastPauseDuration = previousNote.durationInBeats * durationMult
      }
      else {
        val pitchInterval = intervalString.split(";")(0).toInt
        val durationChangeString = intervalString.split(";")(1)
        val durationChange = if (durationChangeString.matches("^1/.*")) { 1 / durationChangeString.split("/")(1).toDouble }
                             else durationChangeString.toDouble

        // determine the real ratio between the previous note / pause duration and this note
        val durationMult = getDurationMult(durationChange, format)
        // the the real duration of current note in milliseconds
        val realDuration = if (lastPauseDuration == 0.0) previousNote.durationInBeats * durationMult
                           else lastPauseDuration * durationMult
        val noteStart = previousNote.end + lastPauseDuration

        val newNote = new Note(
          min(127, max(0, previousNote.pitch + pitchInterval)), noteStart, noteStart + realDuration, tempo)
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
    val name = if (args.size > 0) args(0) else "melody"
    var count = 0

    for( ln <- io.Source.stdin.getLines ) {
      melodyFromSRILMSentence(ln, LMFormat.Round0Rat).saveAsMidi(name + count + ".mid")
      count += 1
    }
  }
}
