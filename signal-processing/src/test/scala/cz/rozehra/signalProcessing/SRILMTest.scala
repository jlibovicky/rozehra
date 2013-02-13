package cz.rozehra.signalProcessing

import languageModeling.{LMFormat, SRILMWrapper}
import org.scalatest.FunSuite
import trackSelection.{Note, Hypothesis}


class SRILMTest extends FunSuite {
  test("srilm rescoring") {
    val lm = new SRILMWrapper("c:\\cygwin\\srilm\\bin\\Release\\ngram.exe",
       "C:\\MFF\\rozehra\\all-rat1.lm.gz", LMFormat.Round1Rat)

    val testHypothesis = Seq(
      new Hypothesis(Seq(new Note(42, 0, 0.1), new Note(44, 0.1, 0.2), new Note(46, 0.2, 0.3)), 0.6),
      new Hypothesis(Seq(new Note(65, 0, 1), new Note(66, 1.2, 2.2)), 0.4)
    )

    val rescored = lm.rescoreNBest(testHypothesis)
    println(rescored.mkString("\n"))
    val rescored2 = lm.rescoreNBest(testHypothesis)
    println(rescored2.mkString("\n"))
  }
}
