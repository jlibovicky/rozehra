package cz.rozehra.signalProcessing.languageModeling

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import cz.rozehra.signalProcessing.trackSelection.{Note, Hypothesis}
import cz.rozehra.signalProcessing.languageModeling.LMFormat.LMFormat
import scala.math.{round, log}
//import sys.process._

class SRILMWrapper(val pathToNgram: String, val pathToModel: String, val lmFormat: LMFormat) extends LanguageModel  {
  def this(pathToModel: String, lmFormat: LMFormat) = this("ngram", pathToModel, lmFormat)

  // start the server process
  private val serverProcess = new ProcessBuilder(pathToNgram, "-order", "9", "-lm",
    pathToModel, "-server-port", "19000").start
  serverProcess.getErrorStream.read // wait until the server is started


  /**
   * Rescores the list of hypothesis and returns it in the new order.
   * @param nBest Original list of hypotheses with internal scores
   * @return New list of hypotheses
   */
  def rescoreNBest(nBest: Iterable[Hypothesis]): Seq[Hypothesis] = {
    val rescoreProcess = new ProcessBuilder(pathToNgram, "-order", "9", "-use-server", "19000", "-rescore", "-").start()
    val rescoreWriter = new BufferedWriter(new OutputStreamWriter(rescoreProcess.getOutputStream))
    val rescoreReader = new BufferedReader(new InputStreamReader(rescoreProcess.getInputStream))

    val stringToHypothesis = collection.mutable.Map.empty[String, Hypothesis]
    for (hypothesis <- nBest) {
      val SRILMString = hypothesis.SRILMString(lmFormat)
      stringToHypothesis += ((SRILMString, hypothesis))
      rescoreWriter.write("(" + round(hypothesis.score) + ") " + SRILMString)
      println("\t\t> (" + round(hypothesis.score) + ") " + SRILMString)
      rescoreWriter.newLine
    }
    rescoreWriter.close

    var reWeightedHypotheses = Seq.empty[(Double, Hypothesis)]
    for (i <- 0 until nBest.size) {
      val line = rescoreReader.readLine()
      val parsedLine = line.split(" ")

      val logScore = parsedLine(1).toDouble
      val notesCount = parsedLine(2).toInt
      val SRILMString = parsedLine.drop(3).mkString(" ")

      reWeightedHypotheses :+= (logScore / notesCount, stringToHypothesis(SRILMString))
    }

    reWeightedHypotheses.sortBy( -_._1).unzip._2
  }

  /**
   * Gets the the note that most likely follows the given hypothesis
   * @param hypothesis A hypothesis which is a sequence of notes
   * @return Most likely following of the given hypothesis
   */
  def mostLikelyNext(hypothesis: Hypothesis): Note = ???

  override def finalize() { serverProcess.destroy() }
}
