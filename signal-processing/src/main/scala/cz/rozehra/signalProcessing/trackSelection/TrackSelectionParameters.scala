package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.languageModeling._

object TrackSelectionParameters {
  val octavePenalty = 0.2
  val durationTolerance = 0.01
  val timeStep = 0.02
  val durationWeight = 0.3 // it's an exponent

  val lowestNote = 43
  val highestNote = 75

  val lmFormat = LMFormat.Round0Rat
  //val languageModel: LanguageModel = EmptyModel
  val languageModel : LanguageModel = new SRILMWrapperRescore("c:\\cygwin\\srilm\\bin\\Release\\ngram.exe",
      "C:\\MFF\\rozehra\\all-rat0.lm.gz", lmFormat)
  val previousTracksToFollow = 3
  val nBestSize = 20
  val edgeCandidatesCount = 6

}
