package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.languageModeling.{LMFormat, SRILMWrapper, LanguageModel}
import cz.rozehra.signalProcessing.partialtracking.Track
import math.{abs, log, min}


object TrackSelection {
  private val languageModel = TrackSelectionParameters.languageModel
  private val lmFormat = TrackSelectionParameters.languageModel
  private val previousTracksToFollow = TrackSelectionParameters.previousTracksToFollow
  private val nBestSize = TrackSelectionParameters.nBestSize
  private val edgeCandidatesCount = TrackSelectionParameters.edgeCandidatesCount

  def run(tracks: Seq[SearchTrack]): Seq[Hypothesis] = {
    val allHypoheses = iterateTrackSelection(Seq.empty, tracks, 0, Set.empty).map(_.hypotheses).flatten.
      groupBy(_.toString).map(_._2.maxBy(_.score))

    languageModel.rescoreNBest(allHypoheses).take(min(allHypoheses.size, 2 * nBestSize))
  }

  def iterateTrackSelection(processedTracks: Seq[SearchTrackWithHypotheses],
                            todoTracks: Seq[SearchTrack], count: Int,
                            finalCandidates: Set[SearchTrackWithHypotheses]): Set[SearchTrackWithHypotheses] = {
    if (todoTracks.isEmpty) finalCandidates
    else {
      val thisTrack = todoTracks.head

      // find given number of track whose ends are closest to the start of the one being processed
      val predecessors = processedTracks.sortBy( t => abs(t.end - thisTrack.start)).
        take(min(processedTracks.size, previousTracksToFollow))

      val newHypotheses = collection.mutable.Set.empty[Hypothesis]
      for (track <- predecessors.par; hypothesis <- track.hypotheses.par;
           (note, noteScore) <- thisTrack.notePossibilities.par) {
        // if necessary shorten the previous note to prevent overlap
        val noteSeq =
          if (hypothesis.notes.last.end <= note.start) hypothesis.notes :+ note
          else hypothesis.notes.dropRight(1) :+ hypothesis.notes.last.shiftStart(note.start) :+ note
        // new hypothesis with cumulative score
        newHypotheses += new Hypothesis(noteSeq, hypothesis.score + bytelog(noteScore))
      }

      print("   >>> generated hypotheses: " + newHypotheses.size + ", ")

      // there may the same hypothesis created different way ... tak those with maximum score
      val uniqueHypotheses = newHypotheses.groupBy(_.toString).map( _._2.maxBy(_.score) )

      println(uniqueHypotheses.size + " of them unique")

      // rescore the hypotheses using the language model and keep the best ones
      val newNBest = languageModel.rescoreNBest(uniqueHypotheses).take(min(nBestSize, uniqueHypotheses.size))

      // if it is one of the first tracks, add the
      val passOnHypotheses =
        if (count < edgeCandidatesCount) {
          val beginnings = thisTrack.notePossibilities.map(
            n => new Hypothesis(Seq(n._1), bytelog(n._2))).sortBy(_.score)
          beginnings.take(min(beginnings.size, 2 * nBestSize)) ++ newNBest
        }
        else newNBest

      val thisTrackProcessed = new SearchTrackWithHypotheses(thisTrack, passOnHypotheses)
      val newProcessedTracks = processedTracks.filter( _.end >= predecessors.head.end) :+ thisTrackProcessed

      // if we are heading towards the end of the list of unprocessed track, start to
      // keep the finisehd ones as candidates to be the last ones
      val newFinalCandidates =
        if (todoTracks.size <= edgeCandidatesCount) finalCandidates + thisTrackProcessed
        else finalCandidates

      iterateTrackSelection(newProcessedTracks, todoTracks.drop(1), count + 1, newFinalCandidates)
    }
  }

  private def bytelog(p: Double) = log(p) / log(1.0001) / 1024

  /**
   * Converts the track from partial tracking to track for the searching algorithm
   * @param tracks Track from the partial tracking
   * @param spectrumRate Number of spectra per second in the source spectrogram
   * @return Searching tracks sorted by the time of start
   */
  def convertTrackToSearchTracks(tracks: Set[Track], spectrumRate: Double): Seq[SearchTrack] =
    (for (track <- tracks.toSeq)
      yield new SearchTrack(track.frequencies, track.start / spectrumRate, track.end / spectrumRate)).
    sortBy(_.start)
}
