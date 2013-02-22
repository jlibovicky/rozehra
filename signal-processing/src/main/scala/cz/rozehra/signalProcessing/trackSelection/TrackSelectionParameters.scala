package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.languageModeling.{LMFormat, SRILMWrapper, LanguageModel}

object TrackSelectionParameters {
  val octavePenalty = 0.5
  val durationTolerance = 0.05
  val timeStep = 0.02
  val durationWeight = 1.0 // it's an exponent

  val lmFormat = LMFormat.Round0Rat
  val languageModel : LanguageModel = new SRILMWrapper("c:\\cygwin\\srilm\\bin\\Release\\ngram.exe",
      "C:\\MFF\\rozehra\\all-rat0.lm.gz", lmFormat)
  val previousTracksToFollow = 3
  val nBestSize = 20
  val edgeCandidatesCount = 6

}
