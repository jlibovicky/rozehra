package cz.rozehra.signalProcessing.languageModeling

import java.io._
import cz.rozehra.signalProcessing.trackSelection.{Note, Hypothesis}
import cz.rozehra.signalProcessing.languageModeling.LMFormat.LMFormat
import scala.math.{round, log10}
import util.control.Breaks
import util.matching.Regex
import util.Random

class SRILMWrapperPpl(val pathToNgram: String, val pathToModel: String, val lmFormat: LMFormat) extends LanguageModel  {
  def this(pathToModel: String, lmFormat: LMFormat) = this("ngram", pathToModel, lmFormat)

  val pathToLogFile = "srilm-last.log"
  // start the server process
  //private val serverProcess = new ProcessBuilder(pathToNgram, "-order", "9", "-lm",
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
      val filename = "srilm" + new Random().nextInt();

      val ngramWriter = new BufferedWriter(new OutputStreamWriter(new FileOutputStream(filename)))

      val stringToHypothesis = collection.mutable.Map.empty[String, Hypothesis]
      for (hypothesis <- nBest) {
        val SRILMString = hypothesis.SRILMString(lmFormat)
        stringToHypothesis += ((SRILMString, hypothesis))
        ngramWriter.write(SRILMString + "\n")
      }
      ngramWriter.flush
      ngramWriter.close

      val ngramProcess = new ProcessBuilder(pathToNgram, "-order", "9", "-use-server", "19000",
        "-ppl", filename, "-debug", "1", "-no-eos").start()

      val ngramReader = new BufferedReader(new InputStreamReader(ngramProcess.getInputStream))
      new dummyReader(ngramProcess.getErrorStream).start()

      var reWeightedHypotheses = List.empty[(Double, Hypothesis)]
      val regexp = """.* zeroprobs, logprob= (.*) ppl= .* ppl1= .*""".r
      for (hypothesis <- nBest) {
        val l1 = ngramReader.readLine()
        val l2 = ngramReader.readLine()
        val result = ngramReader.readLine()
        val logProb: Double = (regexp findFirstIn result match {
          case Some(regexp(logProbString)) => logProbString.toDouble }) / (hypothesis.notes.size - 1)
        val byteLog = 1024.0 * logProb / log10(1.0001)
        reWeightedHypotheses ::= ((hypothesis.normScore + byteLog, hypothesis))
        ngramReader.readLine()
      }
      ngramReader.close()
      new File(filename).delete()

      if (reWeightedHypotheses.isEmpty) nBest.toSeq.sortBy(-_.actualScore)
      else reWeightedHypotheses.sortBy( -_._1).unzip._2.toSeq
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
