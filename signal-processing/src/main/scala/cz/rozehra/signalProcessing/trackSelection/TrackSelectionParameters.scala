package cz.rozehra.signalProcessing.trackSelection

import cz.rozehra.signalProcessing.languageModeling.{LMFormat, SRILMWrapper, LanguageModel}

object TrackSelectionParameters {
  val octavePenalty = 0.5
  val durationTolerance = 0.1
  val timeStep = 0.02
  val durationWeight = 1.0 // it an exponent

  val lmFormat = LMFormat.Round1Rat
  val languageModel : LanguageModel = new SRILMWrapper("c:\\cygwin\\srilm\\bin\\Release\\ngram.exe",
      "C:\\MFF\\rozehra\\all-rat1.lm.gz", lmFormat)
  val previousTracksToFollow = 6
  val nBestSize = 12
  val edgeCandidatesCount = 6

}
