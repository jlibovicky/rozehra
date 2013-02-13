package cz.rozehra.midi

import javax.sound.midi._
import scala.math._
import java.text.DecimalFormat
import java.io.File


class Melody (val notes: Array[Note]) extends Serializable {
  def this(notes: Seq[Note]) = this(notes.toArray)
  def durationInBeats = notes.last.end - notes.head.start

  private val ticksPerBeat = 24
  private lazy val midiSequence: Sequence = {
    val sequence = new Sequence(Sequence.PPQ, ticksPerBeat)
    val track = sequence.createTrack
    val startPenalty = notes.head.start

    //****  General MIDI sysex -- turn on General MIDI sound set  ****
    val b = Array[Byte](0xF0.asInstanceOf[Byte], 0x7E, 0x7F, 0x09, 0x01, 0xF7.asInstanceOf[Byte])
    val sm = new SysexMessage()
    sm.setMessage(b, 6)
    track.add(new MidiEvent(sm, 0l))

    //****  set tempo (meta event)  ****
    val mt = new MetaMessage()
    val bt = Array[Byte](0x02, 0x00.asInstanceOf[Byte], 0x00)
    mt.setMessage(0x51 ,bt, 3)
    track.add(new MidiEvent(mt, 0l))

    //****  set track name (meta event)  ****
    val mt2 = new MetaMessage()
    val trackName = new String("melody")
    mt2.setMessage(0x03, trackName.getBytes, trackName.length)
    track.add(new MidiEvent(mt2, 0l))

    //****  set omni on  ****
    val mm = new ShortMessage()
    mm.setMessage(0xB0, 0x7D,0x00)
    track.add(new MidiEvent(mm, 0l))

    //****  set poly on  ****
    val mm2 = new ShortMessage()
    mm2.setMessage(0xB0, 0x7F,0x00)
    track.add(new MidiEvent(mm2, 0l))

    //****  set instrument to Piano  ****
    val mm3 = new ShortMessage()
    mm3.setMessage(0xC0, 0x00, 0x00)
    track.add(new MidiEvent(mm3, 0l))


    for (note <- notes) {
      //****  note on
      val noteOn = new ShortMessage()
      noteOn.setMessage(0x90, note.pitch, 0x60)
      track.add(new MidiEvent(noteOn, ((note.start - startPenalty) * ticksPerBeat).asInstanceOf[Long]))

      //**** note off
      val noteOff = new ShortMessage()
      noteOff.setMessage(0x80, note.pitch ,0x40)
      track.add(new MidiEvent(noteOff, ((note.end - startPenalty) * ticksPerBeat).asInstanceOf[Long] ))
    }

    //****  set end of track (meta event) 19 ticks later  ****
    val mt3 = new MetaMessage()
    mt3.setMessage(0x2F, Array.empty[Byte], 0)
    track.add(new MidiEvent(mt, ((notes.last.end - startPenalty) * ticksPerBeat).asInstanceOf[Long]))

    sequence
  }

  def play {
    val sequencer = MidiSystem.getSequencer
    sequencer.open()
    sequencer.setSequence(midiSequence)
    sequencer.start()
  }

  def saveAsMidi(path: String) {
    MidiSystem.write(midiSequence, 1, new File(path))
  }

  import LMFormat._

  private val noPlace = new DecimalFormat("#")
  private val onePlace = new DecimalFormat("#.#")
  private val twoPlaces = new DecimalFormat("#.##")

  private def formatTime(format: LMFormat, number: Double): String = {
    def notNegativeZero(num: Double) = if (num == -0.0) 0.0 else num
    if (format == Round1Rat) {
      onePlace.format(notNegativeZero(number))
    }
    else if (format == Round0Rat) {
      noPlace.format(notNegativeZero(number))
    }
    else if (format == Round2Rat) {
      twoPlaces.format(notNegativeZero(number))
    }
    else if (format == Round1Log) {
      onePlace.format((notNegativeZero(log(number) / log(2))))
    }
    else if (format == Round2Log) {
      twoPlaces.format((notNegativeZero(log(number) / log(2))))
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
        builder ++= formatTime(format, pauseDuration / previousNote.durationInMillis)

        builder ++= " "
        previousDuration = pauseDuration
      }

      val modifiedNote = if (pauseDuration > 0 && pauseDuration < 30) note.shiftStart(previousNote.end)
                         else note

      builder ++= (previousNote.pitch - note.pitch).toString
      builder ++= ";"

      builder ++= formatTime(format, modifiedNote.durationInMillis / previousDuration)

      builder ++= " "
      previousNote = note
    }

    builder.toString()
  }

  def getMelodyLength: Double = {
    val notesLength = notes.foldLeft(0.0)( _ + _.durationInMillis )
    val spacesLength = (notes.take(notes.size - 1) zip notes.drop(1)).map(
      s => (s._1.tempo + s._2.tempo) / 2 * (s._2.start - s._1.end) / 1000.0).foldLeft(0.0)(_ + _)
    spacesLength + notesLength
  }
}
