package cz.rozehra.signalProcessing.languageModeling

import cz.rozehra.signalProcessing.trackSelection.{Note, Hypothesis}

object EmptyModel extends LanguageModel {
  /**
   * Just keeps the hypotheses in the order they original have by the
   * @param nBest Original list of hypotheses with internal scores
   * @return New list of hypotheses
   */
  def rescoreNBest(nBest: Iterable[Hypothesis]): Seq[Hypothesis] =
    nBest.toSeq.sortBy(-_.normScore)


  /**
   * Gets the the note that most likely follows the given hypothesis
   * @param hypothesis A hypothesis which is a sequence of notes
   * @return Most likely following of the given hypothesis
   */
  def mostLikelyNext(hypothesis: Hypothesis): Note = ???
}
