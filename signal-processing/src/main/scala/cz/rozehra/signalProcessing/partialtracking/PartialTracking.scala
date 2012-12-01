package cz.rozehra.signalProcessing

import partialtracking.Track
import scala._
import scala.math._


package object partialtracking {
  val medianMultiply: Double = 500.0

  val toneTolerance: Double = 0.6
  val maximumWithoutUpdate: Int = 3
  val minimumTrackDensity: Double = 0.7
  val minimumTrackDuration: Int = 5

  def findPeaksInSpectrum[T <: Double](spectrum: Spectrum[T]): Set[Double] = {

    val median = (spectrum.amplitudes.sortWith( (e1, e2) => e1 < e2))(spectrum.amplitudes.size / 2)

    // spectrum after filtering out values less than median
    var peaks = Set.empty[Frequency]
    val s = spectrum.amplitudes.map( a => if (a > median * medianMultiply) a else 0.0)

    var inNonZeroArea = false
    var tmpMaxIndex = -1

    for (i <- 0 until s.size - 1) {
      if (s(i) > 0 && !inNonZeroArea) {
        inNonZeroArea = true
        tmpMaxIndex = i
      }
      else if (s(i) == 0.0 && inNonZeroArea) {
        inNonZeroArea = false
        peaks += tmpMaxIndex.asInstanceOf[Double] * spectrum.bandWidth
        tmpMaxIndex = -1
      }
      else if (s(i) > 0 && inNonZeroArea) {
        if (s(i) > s(tmpMaxIndex)) tmpMaxIndex = i
      }
    }

    peaks
  }

  /**
   * Updates current tracks
   * @param tracks
   * @param peaks
   * @param currentTime
   * @return
   */
  private def updateTrackByPeaks(tracks: List[Track], peaks: List[Frequency], currentTime: Int): List[Track] = {
    /**
     * Adds one particular peak to a suitable track from the list or founds a new track if there isn't such track.
     * Suitable means it has a right frequencies and hasn't been updated in this round (= current time)
     * @param tracks List of track to be updated
     * @param peak Peak to be added
     * @return
     */
    def addPeakToListOfTracks(tracks: List[Track], peak: Frequency): List[Track] = {
      // goes through existing track and adds the the peak frequency to a track when it reaches
      // a suitable one
      def addPeakToListOfTracks0(tracks: List[Track], accu: List[Track]): List[Track] = {
        tracks match {
          case Nil => new Track(currentTime, currentTime, List(peak), currentTime) :: accu
          case track :: rest => {
            if (track.end == currentTime) addPeakToListOfTracks0(rest, track :: accu)
            else if (12 * abs(log(track.averageFrequency /peak) / log(2.0)) < toneTolerance)
              rest ++ (track.addPeak(peak, currentTime) :: accu)
            else addPeakToListOfTracks0(rest, track :: accu)
          }
        }
      }
      addPeakToListOfTracks0(tracks, Nil)
    }


    // first add peaks to existing tracks
    val tracksWithAddedPeaks = peaks.foldLeft(tracks)( (tracks, peak) => addPeakToListOfTracks(tracks, peak))
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
   * @param spectra List of spectra to be processed
   * @return List of tracks
   */
  def partialTracking[T <: Double](spectra: List[Spectrum[T]]): Set[Track] = {
    def partialTracking0(spectra: List[Spectrum[T]], activeTracks: List[Track],
                         archivedTracks: Set[Track], currentTime: Int): Set[Track] = {
      spectra match {
        case Nil => findValuableTrackAtTheEnd(activeTracks) ++ archivedTracks
        case spectrum :: rest => {
          val peaks = findPeaksInSpectrum(spectrum)
          val updatedTracks = updateTrackByPeaks(activeTracks, peaks.toList, currentTime)
          val (newActive, toArchive) = filterOutFinishedTracks(updatedTracks, currentTime)

          partialTracking0(rest, newActive, archivedTracks ++ toArchive, currentTime + 1)
        }
      }
    }
    partialTracking0(spectra, Nil, Set.empty[Track], 0)
  }
}
