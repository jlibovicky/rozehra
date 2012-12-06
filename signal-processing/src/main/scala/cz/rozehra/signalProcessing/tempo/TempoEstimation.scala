package cz.rozehra.signalProcessing.tempo

import cz.rozehra.signalProcessing._
import scala.math._

object TempoEstimation {
  val windowSize = 4096
  val windowShift = 512
  val tempoUpperBound = 5.0

  val minimumRelativeTrackLength = 0.9

  def fluxSpectrogram(energyFlux: TimeDomainWaveForm[EnergyFlux]): Spectrogram[EnergyFlux] = {
    energyFlux.segmentToWindows(windowSize, windowShift).toSpectrogram
  }

  def tempoEstimation(energyFlux: TimeDomainWaveForm[EnergyFlux]): Double = {
    val spectrogram = modifyFluxSpectrogram(fluxSpectrogram(energyFlux))

    val tempoTracks = TempoPartialTracking.partialTracking(spectrogram.spectra)
    val sortedTracks = tempoTracks.toSeq.sortWith( (t1, t2) => t1.averageAmplitude < t2.averageAmplitude )

    sortedTracks(0).averageFrequency
  }

  /**
   * Takes the spectrogram and filters each spectrum such that it begins with the first local minimum
   * (there are zeros before it) and all values after the tempoUpperBound frequency are also set to
   * zero
   * @param fluxSpectrogram Spectrogram of energy flux
   * @return Energy flux spectrogram limit to the "right band width"
   */
  private def modifyFluxSpectrogram(fluxSpectrogram: Spectrogram[EnergyFlux]): Spectrogram[EnergyFlux] = {
    /**
     * Process one spectrum in the previously described way.
     */
    def processSpectrum(s: Spectrum[EnergyFlux]): Spectrum[EnergyFlux] = {
      val indexOfTempoUpperBound = ceil(tempoUpperBound / s.bandWidth).asInstanceOf[Int]

      def findFirstLocalMinimum(i: Int): Int = {
        if (i == indexOfTempoUpperBound) indexOfTempoUpperBound
        else if (s.amplitudes(i - 1) > s.amplitudes(i) && s.amplitudes(i) < s.amplitudes(i + 1)) i
        else findFirstLocalMinimum(i + 1)
      }

      val firstLocalMinimum = findFirstLocalMinimum(1)

      val newAmplitudes = IndexedSeq.fill[EnergyFlux](firstLocalMinimum)(0.0) ++
        s.amplitudes.slice(firstLocalMinimum, indexOfTempoUpperBound) ++
        IndexedSeq.fill[EnergyFlux](s.amplitudes.size - indexOfTempoUpperBound)(0.0)

      new Spectrum[EnergyFlux](s.withWindowShift, s.bandWidth, newAmplitudes)
    }

    val newListOfSpectra = fluxSpectrogram.spectra.map( s => processSpectrum(s))

    new Spectrogram[EnergyFlux](fluxSpectrogram.spectrumRate, newListOfSpectra,
      fluxSpectrogram.signalWindowSize, fluxSpectrogram.signalWindowShift)
  }

  def main(args: Array[String]) = {
    for (arg <- args) {
      val wave = new WaveFileReader(arg)
      val energyFlux = EnergyFluxComputation.computeEnergyFlux(wave.toTimeDomainWaveForm)
      val tempo = tempoEstimation(energyFlux) * 60
      println(arg + "\t" + tempo)
    }
  }

}


