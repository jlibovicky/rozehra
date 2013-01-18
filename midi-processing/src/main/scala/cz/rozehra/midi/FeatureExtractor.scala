package cz.rozehra.midi

import javax.sound.midi._
import java.io.File
import math._
import weka.core.{Instances, FastVector, Attribute}

object FeatureExtractor {
  val NOTE_ON = 0x90
  val NOTE_OFF = 0x80

  def parseMidiFile(file: File, arffDataSheet: Instances): Seq[TrackVector] = {
    //println(file.getName)
    val midiTracks = MidiSystem.getSequence(file).getTracks

    val notNormalizedVectors = midiTracks.map(processTrack).filter(_ != null)
    val maximumTrack = getMaximumVector(notNormalizedVectors)
    val minimumTrack = getMinimumVector(notNormalizedVectors)

    val normalizedTrackVectors = notNormalizedVectors.map(_.addNormalized(minimumTrack, maximumTrack))
    generateARFF(arffDataSheet, normalizedTrackVectors)
    normalizedTrackVectors.toSeq
  }

  private def processTrack(track: Track): TrackVector =  {
    var openNotes =  collection.mutable.Map.empty[Int, Note]
    var finishedNotes = collection.mutable.Set.empty[Note]

    var melody = false
    var startTime = -1l
    var endTime = -1l
    var currentTempo: Long = 500000

    // ... we should count time when there's silence ot be able to compute
    //  the occupation rate
    var lastSilenceStart: Long = 0
    var silenceTime: Long = 0

    // ... and the same with time where more notes play simultaneously
    //  to compute the polyphony rate
    var lastPolyphonyStart: Long = 0
    var polyphonyTime: Long = 0

    for (event <- (0 until track.size) map (track.get(_))) {
      val time = event.getTick
      if (startTime == -1) startTime = time
      // meta messages
      if (event.getMessage.isInstanceOf[MetaMessage]) {
        val msg = event.getMessage.asInstanceOf[MetaMessage]

        if (msg.getType == 3) { // = track name
          val trackName = new String(msg.getData)
          //println(trackName)
          melody = trackName.matches(".*--BBOK.*")
        }
        else if (msg.getType == 81) { // = tempo
          currentTempo = msg.getData()(0).asInstanceOf[Long] * 65536 +
                         msg.getData()(1).asInstanceOf[Long] * 256 +
                         msg.getData()(2)
        }
        else if (msg.getType == 47) { // = end of track
          endTime = time
        }
      }
      // actual note messages
      else if (event.getMessage.isInstanceOf[ShortMessage]) {
        val msg = event.getMessage.asInstanceOf[ShortMessage]

        if (msg.getCommand == NOTE_ON && msg.getData2 > 0) {
          if (openNotes.isEmpty) silenceTime += time - lastSilenceStart
          if (openNotes.size == 1) lastPolyphonyStart = time
          openNotes += ((msg.getData1, new Note(msg.getData1, time, currentTempo)))
        }
        else if ((msg.getCommand == NOTE_OFF || msg.getCommand == NOTE_ON && msg.getData2 == 0) && openNotes.contains(msg.getData1))  {
          finishedNotes += openNotes(msg.getData1).finished(time)
          // start a period of silence =>
          if (openNotes.isEmpty) lastSilenceStart = time
          // number of open notes has decreased from 2 to 1 =>
          if (openNotes.size == 1) polyphonyTime += time - lastPolyphonyStart
        }
      }

    }

    if (finishedNotes.size < 2) null
    else {
      // now we can compute statistics from the collected notes
      val occupationRate = if(endTime == startTime) 0
                           else 1d - silenceTime.asInstanceOf[Double] / (endTime - startTime)
      val polyphonyRate = if(endTime == startTime) 0
                          else polyphonyTime.asInstanceOf[Double] / (endTime - startTime)

      // pith
      val pitches = finishedNotes.map(_.pitch)
      val highestPitch = pitches.max
      val lowestPitch = pitches.min
      val pitchMean = meanInt(pitches)
      val pitchDeviation = stdevInt(pitchMean, pitches)

      // intervals
      val sortedNotes = finishedNotes.toSeq.sortBy(_.start)
      val intervals = (sortedNotes.dropRight(1) zip sortedNotes.drop(1)).map((pair) => pair._2.pitch - pair._1.pitch)
      val numberOfDifferentIntervals = intervals.foldLeft(Set.empty[Int])( _ + _).size
      val biggestInterval = intervals.max
      val smallestInterval = intervals.min
      val intervalMean = meanInt(intervals)
      val intervalDeviation = stdevInt(intervalMean, intervals)
      val intervalMode = mode(intervals)

      // note durations
      val durations = finishedNotes.map(_.durationInBeats)
      val longestDuration = durations.max
      val shortestDuration = durations.min
      val durationMean = mean(durations)
      val durationDeviation = stdev(durationMean, durations)

      new TrackVector(sortedNotes, melody, endTime - startTime, finishedNotes.size, occupationRate, polyphonyRate,
        highestPitch, lowestPitch, pitchMean, pitchDeviation, numberOfDifferentIntervals,
        biggestInterval, smallestInterval, intervalMean, intervalMode, intervalDeviation,
        longestDuration, shortestDuration, durationMean,
        durationDeviation)
    }
  }

  def meanInt(set: Iterable[Int]): Double = set.foldLeft(0.0)(_ + _.asInstanceOf[Double]) / set.size
  def mean(set: Iterable[Double]): Double = set.foldLeft(0.0)(_ + _) / set.size

  def stdevInt(mean: Double, set: Iterable[Int]) = sqrt(set.map(n => pow(n - mean, 2.0)).foldLeft(0.0)(_ + _) / set.size)
  def stdev(mean: Double, set: Iterable[Double]) = sqrt(set.map(n => pow(n - mean, 2.0)).foldLeft(0.0)(_ + _) / set.size)

  def mode(set: Iterable[Int]) = set.foldLeft(Map.empty[Int, Int])(
    (map, interval: Int) => if (map.contains(interval)) map.updated(interval, map(interval) + 1)
                       else map + ((interval, 1))
  ).maxBy(_._2)._1

  private def generateARFF(data: Instances, tracks: Iterable[TrackVector]) { tracks.foreach(_.writeARFF(data)) }

  // !! an ugly partially automatically generated code starts ....
  private def getMinimumVector(tracks: Iterable[TrackVector]): TrackVector = {
    new TrackVector(
      null, false,
      tracks.minBy(_.normalizedDuration).normalizedDuration,
      tracks.minBy(_.numberOfNotes).numberOfNotes,
      tracks.minBy(_.occupationRate).occupationRate,
      tracks.minBy(_.polyphonyRate).polyphonyRate,
      tracks.minBy(_.highestPith).highestPith,
      tracks.minBy(_.lowestPitch).lowestPitch,
      tracks.minBy(_.pitchMean).pitchMean,
      tracks.minBy(_.pitchDeviation).pitchDeviation,
      tracks.minBy(_.numberOfDifferentIntervals).numberOfDifferentIntervals,
      tracks.minBy(_.largestInterval).largestInterval,
      tracks.minBy(_.smallestInterval).smallestInterval,
      tracks.minBy(_.intervalMean).intervalMean,
      tracks.minBy(_.intervalMode).intervalMode,
      tracks.minBy(_.intervalDeviation).intervalDeviation,
      tracks.minBy(_.longestNote).longestNote,
      tracks.minBy(_.shortestNote).shortestNote,
      tracks.minBy(_.durationMean).durationMean,
      tracks.minBy(_.durationDeviation).durationDeviation
    )
  }

  private def getMaximumVector(tracks: Iterable[TrackVector]): TrackVector = {
    new TrackVector(
      null, false,
      tracks.maxBy(_.normalizedDuration).normalizedDuration,
      tracks.maxBy(_.numberOfNotes).numberOfNotes,
      tracks.maxBy(_.occupationRate).occupationRate,
      tracks.maxBy(_.polyphonyRate).polyphonyRate,
      tracks.maxBy(_.highestPith).highestPith,
      tracks.maxBy(_.lowestPitch).lowestPitch,
      tracks.maxBy(_.pitchMean).pitchMean,
      tracks.maxBy(_.pitchDeviation).pitchDeviation,
      tracks.maxBy(_.numberOfDifferentIntervals).numberOfDifferentIntervals,
      tracks.maxBy(_.largestInterval).largestInterval,
      tracks.maxBy(_.smallestInterval).smallestInterval,
      tracks.maxBy(_.intervalMean).intervalMean,
      tracks.maxBy(_.intervalMode).intervalMode,
      tracks.maxBy(_.intervalDeviation).intervalDeviation,
      tracks.maxBy(_.longestNote).longestNote,
      tracks.maxBy(_.shortestNote).shortestNote,
      tracks.maxBy(_.durationMean).durationMean,
      tracks.maxBy(_.durationDeviation).durationDeviation
    )
  }
}
