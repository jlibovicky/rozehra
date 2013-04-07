package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.languageModeling._

object TrackSelectionParameters {
  var srilmPort: Int = 19000

  var octavePenalty = 0.5
  var durationTolerance = 0.01
  var timeStep = 0.05
  var durationWeight = 2.0 // it's an exponent
  var densityBonification = 4.0

  var lowestNote = 43
  var highestNote = 75

  var lmFormat = LMFormat.None
  var languageModel: LanguageModel = EmptyModel
  //var languageModel : LanguageModel = new SRILMWrapperRescore("c:\\cygwin\\srilm\\bin\\Release\\ngram.exe",
  //    "C:\\MFF\\rozehra\\all-rat0clust.lm.gz", lmFormat)
  val previousTracksToFollow = 3
  var nBestSize = 20
  val edgeCandidatesCount = 6

}
