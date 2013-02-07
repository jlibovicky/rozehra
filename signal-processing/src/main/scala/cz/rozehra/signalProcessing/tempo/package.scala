package cz.rozehra.signalProcessing

import scala.math._

package object tempo {
  /**
   * Performs the rhythmic analysis of the given signal. Computes its tempo (i.e. the number of beats per second
   * and shift of the tempo towards the start of the record) and the times of note onsets.
   * @param signal The sound signal to be analyzed
   * @return A pair of the tempo (the tempo value and its shift towards the start of the signal) and the sequence of
   *         note onsets
   */
  def rhythmicAnalysis(signal: TimeDomainWaveForm[Signal]): (Tempo, Seq[Time]) = {
    val energyFlux = DefaultEnergyFluxComputation.computeEnergyFlux(signal)

    // first compute the times of note onsets ...
    val noteOnsets = NoteOnsetDetection.computeNoteOnsetTimes(energyFlux)
    // ... and the absolute value of tempo
    val tempoValue = TempoEstimation.tempoEstimation(energyFlux)
    val beatPeriod = 1 / tempoValue

    // now we're gonna compute the tempo shift such that the it will minimize
    // the distances of the note onsets from the beats

    // this function gets the cumulative note onsets deviation for a particular value of shift
    def onsetsDeviationFroShift(shift: Time): Time = {
      val normalizedOnsets = noteOnsets.map( o => (o - shift) / beatPeriod )
      normalizedOnsets.fold(0.0)( (sum, o) => sum + abs(o - round(o)) )
    }

    // take 100 values of shift from -p/2 to +p/2 and find the best one
    val deviations = (-50 to 50).toSeq.par.map( i => i.asInstanceOf[Double] / 100 * beatPeriod).
      map( shift => onsetsDeviationFroShift(shift))
    val bestShift = (deviations.zipWithIndex.min._2.asInstanceOf[Double] / 100.0 - 0.5) * beatPeriod

    (new Tempo(tempoValue, bestShift), noteOnsets)
  }
}
