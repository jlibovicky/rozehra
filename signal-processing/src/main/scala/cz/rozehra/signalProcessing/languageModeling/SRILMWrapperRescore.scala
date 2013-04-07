package cz.rozehra.signalProcessing.languageModeling

import java.io._
import cz.rozehra.signalProcessing.trackSelection.{TrackSelectionParameters, Note, Hypothesis}
import cz.rozehra.signalProcessing.languageModeling.LMFormat.LMFormat
import scala.math.{round}
import util.control.Breaks

class SRILMWrapperRescore(val pathToNgram: String, val pathToModel: String, val lmFormat: LMFormat) extends LanguageModel  {
  def this(pathToModel: String, lmFormat: LMFormat) = this("ngram", pathToModel, lmFormat)

  // start the server process
  //private val serverProcess = new ProcessBuilder(pathToNgram, "-order", 10", "-lm",
  //  pathToModel, "-server-port", "19000").start
  //serverProcess.getErrorStream.read // wait until the server is started
  //new dummyReader(serverProcess.getErrorStream).start()
  //new dummyReader(serverProcess.getInputStream).start()

  /**
   * Rescores the list of hypothesis and returns it in the new order.
   * @param nBest Original list of hypotheses with internal scores
   * @return New list of hypotheses
   */
  def rescoreNBest(nBest: Iterable[Hypothesis]): Seq[Hypothesis] = {
    if (nBest.isEmpty) Seq.empty[Hypothesis]
    else {
      val ngramProcess = new ProcessBuilder(pathToNgram, "-order", "10", "-use-server", TrackSelectionParameters.srilmPort,
        "-nbest", "-", "-no-eos").start()
      val ngramWriter = new BufferedWriter(new OutputStreamWriter(ngramProcess.getOutputStream))

      val stringToHypothesis = collection.mutable.Map.empty[String, Hypothesis]
      for (hypothesis <- nBest) {
        val SRILMString = hypothesis.SRILMString(lmFormat)
        stringToHypothesis += ((SRILMString, hypothesis))
        ngramWriter.write("(" + round(hypothesis.normScore) + ") " + SRILMString + "\n")
      }
      ngramWriter.flush
      ngramWriter.close

      val ngramReader = new BufferedReader(new InputStreamReader(ngramProcess.getInputStream))
      new dummyReader(ngramProcess.getErrorStream).start()

      var reWeightedHypotheses = Seq.empty[(Double, Hypothesis)]
      val loop = new Breaks
      loop.breakable(
        while (true)  {
          val line = ngramReader.readLine()
          if (line == null) loop.break()
          //println(line)
          val parsedLine = line.split(" ")

          val logScore = parsedLine(0).toDouble
          val notesCount = parsedLine(2).toInt
          val SRILMString = parsedLine.drop(3).mkString(" ")

          reWeightedHypotheses :+= (logScore / notesCount, stringToHypothesis(SRILMString))
        }
      )
      ngramReader.close()

      if (reWeightedHypotheses.isEmpty) nBest.toSeq.sortBy(-_.actualScore)
      else reWeightedHypotheses.sortBy( -_._1).unzip._2
    }
  }

  /**
   * Gets the the note that most likely follows the given hypothesis
   * @param hypothesis A hypothesis which is a sequence of notes
   * @return Most likely following of the given hypothesis
   */
  def mostLikelyNext(hypothesis: Hypothesis): Note = ???

  //override def finalize() { serverProcess.destroy() }

  private class dummyReader(errOutputStream: InputStream) extends Thread {
    override def run() {
      val errorReader = new BufferedReader(new InputStreamReader(errOutputStream))
      val loop2 = new Breaks
      loop2.breakable(
        while (true)  {
          val line = errorReader.readLine()
          if (line == null) loop2.break()
        }
      )
      errorReader.close()
    }
  }
}
