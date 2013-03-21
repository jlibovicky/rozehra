package cz.rozehra.signalProcessing.partialtracking

import cz.rozehra.signalProcessing._
import math._


abstract trait GenericPartialTracking[T <: Double] {
  def toneTolerance: Double
  def maximumWithoutUpdate: Int
  def minimumTrackDensity: Double
  def minimumTrackDuration: Int


  protected def appendCondition(avgFreq: Double, peak: Double): Boolean =
    12 * abs(log(avgFreq /peak) / log(2.0)) < toneTolerance


  /**
   * Updates current tracks
   * @param tracks
   * @param peaks
   * @param currentTime
   * @return
   */
  private def updateTrackByPeaks(tracks: List[Track], peaks: List[(Frequency, Double)], currentTime: Int): List[Track] = {
    /**
     * Adds one particular peak to a suitable track from the list or founds a new track if there isn't such track.
     * Suitable means it has a right frequencies and hasn't been updated in this round (= current time)
     * @param tracks List of track to be updated
     * @param peak Peak to be added
     * @return
     */
    def addPeakToListOfTracks(tracks: List[Track], peak: Frequency, peakAmplitude: Double): List[Track] = {
      // goes through existing track and adds the the peak frequency to a track when it reaches
      // a suitable one
      def addPeakToListOfTracks0(tracks: List[Track], accu: List[Track]): List[Track] = {
        tracks match {
          case Nil => new Track(currentTime, currentTime, List(peak), List(peakAmplitude), currentTime) :: accu
          case track :: rest => {
            if (track.end == currentTime) addPeakToListOfTracks0(rest, track :: accu)
            else if (appendCondition(track.averageFrequency, peak))
              rest ++ (track.addPeak(peak, peakAmplitude, currentTime) :: accu)
            else addPeakToListOfTracks0(rest, track :: accu)
          }
        }
      }
      addPeakToListOfTracks0(tracks, Nil)
    }


    // first add peaks to existing tracks
    val tracksWithAddedPeaks = peaks.foldLeft(tracks)( (tracks, peak) => addPeakToListOfTracks(tracks, peak._1, peak._2))
    // shift end time of tracks which hasn't been extended in this turn
    tracksWithAddedPeaks.map( track => if(track.end == currentTime) track else track.incEndTime )
  }

  /**
   * It throws away too sparse or too short tracks and returns pair of active tracks
   * and tracks which are O.K. but not active any more.
   * @param tracks List of tracks from the last iteration
   */
  private def filterOutFinishedTracks(tracks: List[Track], currentTime: Int): (List[Track], Set[Track]) = {
    def filterOutFinishedTracks0(tracks: List[Track], accu: (List[Track], Set[Track])): (List[Track], Set[Track]) = {
      tracks match {
        case Nil => accu
        case track :: rest => {

          // track should be terminated (no update for a long time)
          if (currentTime - track.lastFrequencyAddedTime > maximumWithoutUpdate) {
            val terminated = track.terminate
            // if is evaluated as a relevant track (dense enough as well as long enough)
            if (terminated.density >= minimumTrackDensity && terminated.duration > minimumTrackDuration) {
              filterOutFinishedTracks0(rest, (accu._1, accu._2 + terminated ))
            }
            else filterOutFinishedTracks0(rest, accu)
          }
          // track is active ... just keep it in the list
          else filterOutFinishedTracks0(rest, (track :: accu._1, accu._2))
        }
      }
    }
    filterOutFinishedTracks0(tracks, (Nil, Set.empty[Track]))
  }

  /**
   * Takes the list of unfinished track and return those which already fulfills the track conditions
   * (it is used at the very end of partial tracking not to lose good, but still active tracks).
   * @param tracks
   * @return
   */
  private def findValuableTrackAtTheEnd(tracks: List[Track]): Set[Track] = {
    def findValuableTrackAtTheEnd0(tracks: List[Track], accu: Set[Track]): Set[Track] = {
      tracks match {
        case Nil => accu
        case track :: rest => {
          val terminated = track.terminate
          if (terminated.density >= minimumTrackDensity && terminated.duration > minimumTrackDuration)
            findValuableTrackAtTheEnd0(rest, accu + terminated)
          else
            findValuableTrackAtTheEnd0(rest, accu)
        }
      }
    }
    findValuableTrackAtTheEnd0(tracks, Set.empty[Track])
  }

  /**
   * Performs the actual partial tracking algorithm with parameters set in this object.
   * @param peaksList List of selected spectral peaks
   * @return List of tracks
   */
  def partialTracking(peaksList: List[Seq[(Frequency, Double)]]): Set[Track] = {
    def partialTracking0(peaksList: List[Seq[(Frequency, Double)]], activeTracks: List[Track],
                         archivedTracks: Set[Track], currentTime: Int): Set[Track] = {
      peaksList match {
        case Nil => findValuableTrackAtTheEnd(activeTracks) ++ archivedTracks
        case peaks :: rest => {
          val updatedTracks = updateTrackByPeaks(activeTracks, peaks.toList, currentTime)
          val (newActive, toArchive) = filterOutFinishedTracks(updatedTracks, currentTime)

          partialTracking0(rest, newActive, archivedTracks ++ toArchive, currentTime + 1)
        }
      }
    }

    val rawTracks = partialTracking0(peaksList, Nil, Set.empty[Track], 0)
    val tracksToReturn = collection.mutable.Set.empty[Track]

    // take all tracks which are almost the same and merge them
    for ((pitch, tracks) <- rawTracks.groupBy(_.pitch)) {
      var restOfTracks = tracks
      var growingTrack: Track = null
      def overlap(t: Track): Boolean = (t.start < growingTrack.start && t.end > growingTrack.start) ||
                              (t.start < growingTrack.end && t.end > growingTrack.end)

      while(restOfTracks.nonEmpty) {
        growingTrack = restOfTracks.head
        restOfTracks = restOfTracks.tail

        while( restOfTracks.exists(overlap)) {
          val (toMerge, rest) = restOfTracks.partition(overlap)
          growingTrack = toMerge.foldLeft(growingTrack)(_ merge _)
          restOfTracks = rest
        }
        tracksToReturn.add(growingTrack)
      }
    }
    tracksToReturn.toSet
  }
}
