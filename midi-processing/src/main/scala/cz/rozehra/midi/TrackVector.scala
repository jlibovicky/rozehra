 package cz.rozehra.midi

 import weka.core.{Instance, Instances}

 class TrackVector private(
  val notes: Seq[Note],
  val melody: Boolean,
  val normalizedDuration: Double,
  val numberOfNotes: Int,
  val occupationRate: Double,
  val polyphonyRate: Double,

  // pitch
  val highestPith: Int,
  val normHighestPitch: Double,

  val lowestPitch: Int,
  val normLowestPitch: Double,

  val pitchMean: Double,
  val normPitchMean: Double,

  val pitchDeviation: Double,
  val normPitchDeviation: Double,

  // intervals
  val numberOfDifferentIntervals: Int,
  val normNumberOfDifferentIntervals: Double,

  val largestInterval: Int,
  val normLargestInterval: Double,

  val smallestInterval: Int,
  val normSmallestInterval: Double,

  val intervalMean: Double,
  val normIntervalMean: Double,

  val intervalMode: Int,
  val normIntervalMode: Double,

  val intervalDeviation: Double,
  val normIntervalDeviation: Double,

  // duration
  val longestNote: Double,
  val normLongestNote: Double,

  val shortestNote: Double,
  val normShortestNote: Double,

  val durationMean: Double,
  val normDurationMean: Double,

  val durationDeviation: Double,
  val normDurationDeviation: Double) {

  def this(notes: Seq[Note], melody: Boolean, normalizedDuration: Double, numberOfNotes: Int,
    occupationRate: Double, polyphonyRate: Double,
    // following have normalized twins
    highestPith: Int, lowestPitch: Int, pitchMean: Double, pitchDeviation: Double,
    numberOfDifferentIntervals: Int, largestInterval: Int, smallestInterval: Int, intervalMean: Double,
    intervalMode: Int, intervalDeviation: Double, longestNote: Double, shortestNote: Double, durationMean: Double,
    durationDeviation: Double) =
    this(notes, melody, normalizedDuration, numberOfNotes, occupationRate, polyphonyRate, highestPith, Double.MinValue,
    lowestPitch, Double.MinValue, pitchMean, Double.MinValue, pitchDeviation, Double.MinValue,
    numberOfDifferentIntervals, Double.MinValue, largestInterval, Double.MinValue, smallestInterval, Double.MinValue,
    intervalMean, Double.MinValue, intervalMode, Double.MinValue, intervalDeviation, Double.MinValue,
    longestNote, Double.MinValue, shortestNote, Double.MinValue,  durationMean, Double.MinValue,
    durationDeviation, Double.MinValue)


  def addNormalized(minimumValues: TrackVector, maximumValues: TrackVector) = {
    def normalize(value : Double, min : Double, max : Double): Double = (value - min) / (max - min)

    new TrackVector(notes, melody,
      normalize(normalizedDuration, minimumValues.normalizedDuration, maximumValues.normalizedDuration),
      numberOfNotes, occupationRate, polyphonyRate,

      highestPith, normalize(highestPith, minimumValues.highestPith, maximumValues.highestPith),
      lowestPitch, normalize(lowestPitch, minimumValues.lowestPitch, maximumValues.lowestPitch),
      pitchMean, normalize(pitchMean, minimumValues.pitchMean, maximumValues.pitchMean),
      pitchDeviation, normalize(pitchDeviation, minimumValues.pitchDeviation, maximumValues.pitchDeviation),

      numberOfDifferentIntervals, normalize(numberOfDifferentIntervals, minimumValues.numberOfDifferentIntervals,
        maximumValues.numberOfDifferentIntervals),
      largestInterval, normalize(largestInterval, minimumValues.largestInterval, maximumValues.largestInterval),
      smallestInterval, normalize(smallestInterval, minimumValues.smallestInterval, maximumValues.smallestInterval),
      intervalMean, normalize(intervalMean, minimumValues.intervalMean, maximumValues.intervalMean),
      intervalMode, normalize(intervalMode, minimumValues.intervalMode, maximumValues.intervalMode),
      intervalDeviation, normalize(intervalDeviation, minimumValues.intervalDeviation, maximumValues.intervalDeviation),

      longestNote, normalize(longestNote, minimumValues.longestNote, maximumValues.longestNote),
      shortestNote, normalize(shortestNote, minimumValues.shortestNote, maximumValues.shortestNote),
      durationMean, normalize(durationMean, minimumValues.durationMean, maximumValues.durationMean),
      durationDeviation, normalize(durationDeviation, minimumValues.durationDeviation, maximumValues.durationDeviation)
    )

  }

  def writeARFF(data: Instances) {
    val classificationValue: Double = if (melody) ProcessDir.fvMelodyVal.indexOf("YES")
                                      else ProcessDir.fvMelodyVal.indexOf("NO")

    data.add(new Instance(1.0, numbersAsArray.+:(classificationValue)))
  }

 private def numbersAsArray: Array[Double] =
   Array(normalizedDuration, numberOfNotes, occupationRate, polyphonyRate, highestPith, normHighestPitch,
     lowestPitch, normLowestPitch, pitchMean, normPitchMean, pitchDeviation, normPitchDeviation,
     numberOfDifferentIntervals, normNumberOfDifferentIntervals, largestInterval, normLargestInterval,
     smallestInterval, normSmallestInterval, intervalMean, normIntervalMean, intervalMode, normIntervalMode,
     intervalDeviation, normIntervalDeviation, longestNote, normLongestNote, shortestNote, normShortestNote,
     durationMean, normDurationMean, durationDeviation, normDurationDeviation)


}
