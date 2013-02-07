package cz.rozehra.signalProcessing.salienceFunction

import scala.math._
import cz.rozehra.signalProcessing._

class FundamentalsDetector(val spectrum: Spectrum[Signal]) {
  val alpha = 52.0 //27.0  // Hz
  val beta = 320.0 //320.0  // Hz
  val minimumPeriod = spectrum.samplingRate / 2100.0  //  4.2 kHz  ... approx. the highest piano note
  val maximumPeriod = spectrum.samplingRate / 10.0   // 20 Hz ... approx. the lowest piano tone
  val harmonicsCount = 10
  val deltaPeriod = 0.5 //
  val periodPrecision = 1.0 //0.5 // ??? compute how much are these in frequency and think about it
  val foundSoundSubtraction = 0.89 // 1.0
  val gammaForStopMeasure = 0.7
  val maximumFundamentalsInSpectrum = 10

  def gFunction(period: Double, m: Int) =
    (spectrum.samplingRate / period + alpha) / (m * spectrum.samplingRate / period + beta)

  def gForInterval(periodLow: Double, periodUp: Double, m: Int) =
    (spectrum.samplingRate / periodLow + alpha) / (m * spectrum.samplingRate / periodUp + beta)

  def kappaIndexes(period: Double, m: Int) = {
    ((floor(m * spectrum.bandsCount / (period + deltaPeriod / 2)) - 1).asInstanceOf[Int] to
          (ceil(m * spectrum.bandsCount / (period - deltaPeriod / 2)) - 1).asInstanceOf[Int])
      .filter(_ < spectrum.bandsCount)
  }

  def kappaIndexesForInterval(periodLow: Double, periodUp: Double, m: Int) = {
    val period = (periodLow + periodUp) / 2
    val periodIntervalSize = periodUp - periodLow

    ((floor(m * spectrum.bandsCount / (period + periodIntervalSize / 2)) - 1).asInstanceOf[Int] to
      (ceil(m * spectrum.bandsCount / (period - periodIntervalSize / 2)) - 1).asInstanceOf[Int]).
      filter( i => (i < spectrum.bandsCount) && (i > 0))
  }

  /**
   * Computes the salience of a period (corresponding to a fundamental frequency)
   * @param period Signal period
   * @return Salience for given period
   */
  def salienceFunction(period: Time) =
    (2 to harmonicsCount).foldLeft(0.0)( (sum, m) => {
      val amplitudes = kappaIndexes(period, m).map(spectrum.amplitudes(_))
      if (amplitudes.isEmpty) sum
      else sum + gFunction(period, m) * amplitudes.max    })

  /**
   * Estimates the maximum of salience function in a period interval.
   * @param periodLow Lower bound of the period interval.
   * @param periodUp Upper bound the period interval.
   * @return
   */
  def salienceEstimate(periodLow: Double, periodUp: Double, amplitudes: IndexedSeq[Double]) =
    (2 to harmonicsCount ).foldLeft(0.0)( (sum, m) =>  {
      val thisAmplitudes = kappaIndexesForInterval(periodLow, periodUp, m).map(amplitudes(_))
      if (thisAmplitudes.isEmpty) sum
      else sum + gForInterval(periodLow, periodUp, m) * thisAmplitudes.max

    })

  /**
   * Finds the period corresponding to fundamental frequency in the
   * @param amplitudes Sequence of amplitudes from a given spectrum
   * @return Period
   */
  def maximumSearch(amplitudes: IndexedSeq[Double]) = {
    def iterate(tauLow: Double, tauUp: Double): Double =
      if (tauUp - tauLow < periodPrecision) (tauUp + tauLow) / 2
      else {
        val tauMiddle = (tauUp + tauLow) / 2
        if(salienceEstimate(tauLow, tauMiddle, amplitudes) > salienceEstimate(tauMiddle, tauUp, amplitudes))
          iterate(tauLow, tauMiddle)
        else iterate(tauMiddle, tauUp)
      }

    val period = iterate(minimumPeriod, maximumPeriod)
    val salience = salienceFunction(period)
    (period, salience)
  }

  def findFundamentals = {
    def iterate(amplitudes: IndexedSeq[Double], stopMeasure: Double, acc: Seq[(Frequency, Double)]): Seq[(Frequency, Double)] = {
      val newFundamentalPeriod = maximumSearch(amplitudes)

      val newStopMeasure = (acc :+ newFundamentalPeriod).foldLeft(0.0)( (sum, s) => sum + s._2) / pow(acc.size + 1, gammaForStopMeasure)
      if (newStopMeasure <= stopMeasure || acc.size >= maximumFundamentalsInSpectrum) acc
      else {
        val detectedSound = collection.mutable.IndexedSeq.fill(amplitudes.size)(0.0)
        val fundamentalIndex = floor(2 * amplitudes.size / newFundamentalPeriod._1).asInstanceOf[Int]

        detectedSound(fundamentalIndex) = amplitudes(fundamentalIndex)
              for (m <- 2 to harmonicsCount) {
          val index = floor(m * 2 * amplitudes.size / newFundamentalPeriod._1).asInstanceOf[Int]
          if (index < amplitudes.size + 1){
                detectedSound(index) = gFunction(newFundamentalPeriod._1, m) * amplitudes(index)
          }
        }

        val residualAmplitudes = for (i <- 0 until amplitudes.size)
          yield max(0, amplitudes(i) - foundSoundSubtraction * detectedSound(i))

        iterate(residualAmplitudes, newStopMeasure, acc :+ (spectrum.samplingRate / newFundamentalPeriod._1, newFundamentalPeriod._2) )
      }
    }

    iterate(spectrum.amplitudes, Double.MinValue, Seq.empty)
  }
}
