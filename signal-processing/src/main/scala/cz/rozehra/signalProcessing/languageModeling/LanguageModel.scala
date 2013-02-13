package cz.rozehra.signalProcessing.languageModeling

import cz.rozehra.signalProcessing.trackSelection.{Hypothesis, Note}

abstract trait LanguageModel {
  /**
   * Rescores the list of hypothesis and returns it in the new order.
   * @param nBest Original list of hypotheses with internal scores
   * @return New list of hypotheses
   */
  def rescoreNBest(nBest: Iterable[Hypothesis]): Seq[Hypothesis]

  /**
   * Gets the the note that most likely follows the given hypothesis
   * @param hypothesis A hypothesis which is a sequence of notes
   * @return Most likely following of the given hypothesis
   */
  def mostLikelyNext(hypothesis: Hypothesis): Note
}
