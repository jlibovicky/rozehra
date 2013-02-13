package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing._

class SearchTrackWithHypotheses(frequencies: Seq[Frequency], start: Time, end: Time,
                                val hypotheses: Seq[Hypothesis]) extends AbstractSearchTrack(frequencies, start, end){
  def this(track: AbstractSearchTrack, hypotheses: Seq[Hypothesis]) =
    this(track.frequencies, track.start, track.end, hypotheses)

}
