package cz.rozehra.midi


object TrackPostprocessing {
  private def skyline(track: TrackVector): Melody = {
    val notes = track.notes

    // remove everything shorter than 0.02 s
    val withoutShort = notes.filter(_.durationInMillis > 30)

    // keeps notes which are at least at one moment the highest sounding note
    // and gradually adds them to the mutable collection notes to keep
    var notesToKeep = collection.mutable.Seq.empty[Note]
    var playingNotes = collection.mutable.Seq.empty[Note]
    for (note <- withoutShort) {
      val time = note.start
      playingNotes = playingNotes.filter( _.end > time)

      if (playingNotes.isEmpty || note.pitch > playingNotes.maxBy(_.pitch).pitch)
        notesToKeep :+= note

      playingNotes :+= note
    }

    // finds the overlapping notes and shortens them not to overlap
    val skylined =
      if (!notesToKeep.isEmpty) { (
        for (i <- 0 until notesToKeep.size - 1)
          yield if (notesToKeep(i).end > notesToKeep(i + 1).start)
                  notesToKeep(i).shortenTo(notesToKeep(i + 1).start)
                else
                  notesToKeep(i)
        ) :+ notesToKeep.last }
      else Seq.empty

    // again filters out the notes which are too short
    new Melody(skylined.filter(_.durationInMillis > 30))
  }

  private def splitMelodyToNonSilencePieces(melody: Melody): Seq[Melody] = {
    if (melody.notes.isEmpty) Seq.empty
    else {
      var finishedMelodies = Seq.empty[Melody]
      var currentMelodyNotes = Seq(melody.notes(0))

      for (note <- melody.notes.drop(1)) {
        if (note.start - currentMelodyNotes.last.end > 8) {
          finishedMelodies :+= new Melody(currentMelodyNotes)
          currentMelodyNotes = collection.mutable.Seq(note)
        }
        else currentMelodyNotes :+= note
      }
      finishedMelodies :+= new Melody(currentMelodyNotes)

      finishedMelodies.filter(_.durationInBeats > 16) // at least approx. 4 bars
    }
  }

  def takeMelodiesFromTrack(track: TrackVector) = {
    val afterSkyline = skyline(track)
    splitMelodyToNonSilencePieces(afterSkyline)
  }
}
