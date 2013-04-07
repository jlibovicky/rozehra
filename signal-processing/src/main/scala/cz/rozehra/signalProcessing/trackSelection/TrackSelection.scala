package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.languageModeling.{LMFormat, SRILMWrapperRescore, LanguageModel}
import cz.rozehra.signalProcessing.partialtracking.Track
import math.{abs, log, min, pow}


object TrackSelection {
  private val languageModel = TrackSelectionParameters.languageModel
  private val lmFormat = TrackSelectionParameters.lmFormat
  private val previousTracksToFollow = TrackSelectionParameters.previousTracksToFollow
  private val nBestSize = TrackSelectionParameters.nBestSize
  private val edgeCandidatesCount = TrackSelectionParameters.edgeCandidatesCount

  def run(tracks: Seq[SearchTrack]): Seq[Hypothesis] = {
    val allHypotheses = iterateTrackSelection(Seq.empty, tracks, 0, Seq.empty).map(_.hypotheses).flatten.
      groupBy(_.toString).map(_._2.maxBy(_.actualScore))

    languageModel.rescoreNBest(allHypotheses).take(min(allHypotheses.size, 2 * nBestSize))
  }

  private def iterateTrackSelection(processedTracks: Seq[SearchTrackWithHypotheses],
                            todoTracks: Seq[SearchTrack], count: Int,
                            lastTracks: Seq[SearchTrackWithHypotheses]): Seq[SearchTrackWithHypotheses] = {
    if (todoTracks.isEmpty) lastTracks
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
          else (hypothesis.notes.dropRight(1) :+ hypothesis.notes.last.shiftEnd(note.start) :+ note).filter(_.duration > 0.02)
        // new hypothesis with cumulative score
        newHypotheses += new Hypothesis(noteSeq, hypothesis.actualScore + bytelog(noteScore))
      }

      //print("   >>> continuation hypotheses: " + newHypotheses.size + ", ")

      // there may the same hypothesis created different way ... tak those with maximum score
      val uniqueHypotheses = normalizeHypothesesDist[Iterable[Hypothesis]](newHypotheses.groupBy(_.SRILMString(lmFormat)).
        map( _._2.maxBy(_.scorePerNote) ))

//      /print(uniqueHypotheses.size + " of them unique")

      // rescore the hypotheses using the language model and keep the best ones
      val newNBest = languageModel.rescoreNBest(uniqueHypotheses).take(min(nBestSize, uniqueHypotheses.size))

      // if it is one of the first tracks, add the
      val passOnHypotheses = normalizeHypothesesDist[Seq[Hypothesis]](
        if (count < edgeCandidatesCount) {
          val beginnings = thisTrack.notePossibilities.map(
            n => new Hypothesis(Seq(n._1), bytelog(n._2))).sortBy(-_.scorePerNote)
          beginnings.take(min(beginnings.size, 2 * nBestSize)) ++ newNBest
        }
        else newNBest)

      //println(", " + passOnHypotheses.size + " hypotheses kept")
      val thisTrackProcessed = new SearchTrackWithHypotheses(thisTrack, passOnHypotheses)

      // if we are heading towards the end of the list of unprocessed track, start to
      // keep the finished ones as candidates to be the last ones
      val newLastTracks =
        if (lastTracks.size >= edgeCandidatesCount && passOnHypotheses.nonEmpty)
          lastTracks.drop(lastTracks.size - edgeCandidatesCount) :+ thisTrackProcessed
        else if (passOnHypotheses.nonEmpty) lastTracks :+ thisTrackProcessed
        else lastTracks

      // if the track does not contribute with any new hypotheses, don't add it into the completed
      if (passOnHypotheses.isEmpty) iterateTrackSelection(predecessors, todoTracks.drop(1), count, newLastTracks)
      else iterateTrackSelection(predecessors :+ thisTrackProcessed,
                                  todoTracks.drop(1), count + 1, newLastTracks)
    }
  }

  private def bytelog(p: Double): Double = log(p) / log(1.0001) / 1024

  private def normalizeHypothesesDist[T <: Iterable[Hypothesis]](hypotheses: T): Seq[Hypothesis] = {
    // summation of list of very small numbers
    def saveSum(doubles: List[Double]): Double = {
      def saveSum0(doubles: List[Double], acc: List[Double]): Double = {
        doubles match {
          case Nil => acc match { case Nil => 0.0
                                  case List(sum) => sum
                                  case _ => saveSum0(acc, Nil)}
          case n1 :: n2 :: rest => saveSum0(rest, (n1 + n2) :: acc)
          case List(num) => saveSum0(Nil, num :: acc)
        }
      }
      saveSum0(doubles, Nil)
    }

    val sum = saveSum(hypotheses.map( h => pow(1.0001, 1024 * h.scorePerNote )).toList)
    val bytelogSum = bytelog(sum)
    hypotheses.map(_.setNormConstant(bytelogSum)).toSeq
  }

  /**
   * Converts the track from partial tracking to track for the searching algorithm
   * @param tracks Track from the partial tracking
   * @param spectrumRate Number of spectra per second in the source spectrogram
   * @return Searching tracks sorted by the time of start
   */
  def convertTrackToSearchTracks(tracks: Set[Track], spectrumRate: Double): Seq[SearchTrack] =
    (for (track <- tracks.toSeq)
      yield new SearchTrack(track.frequencies, (track.start.toDouble) / spectrumRate, (track.end.toDouble) / spectrumRate)).
    sortBy(_.start)
}
