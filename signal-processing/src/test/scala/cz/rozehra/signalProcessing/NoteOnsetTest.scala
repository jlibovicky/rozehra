package cz.rozehra.signalProcessing

import java.io.InputStream
import org.scalatest.FunSuite
import tempo.{NoteOnsetDetection, Tempo}
import visualization.Visualizer

//import visualization.Visualizer


class NoteOnsetTest extends FunSuite {
   /*test("raw total energy flux") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    val wave = new WaveFileReader(resource);

    NoteOnsetDetection.computeEnergyFlux(wave.toTimeDomainWaveForm).plot
  }  */

  /*test("compute the onsets") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    val wave = new WaveFileReader(resource)
    val spectrogram = wave.toTimeDomainWaveForm.segmentToWindows(1024, 512).toSpectrogram

    val visualzer = new Visualizer()
    visualzer.drawSpectrum(spectrogram)

    val energyFlux = EnergyFluxComputation.computeEnergyFlux(wave.toTimeDomainWaveForm)
    val onsets = NoteOnsetDetection.computeNoteOnsetTimes(energyFlux)
    onsets.foreach( t => println(t))
    visualzer.drawNoteOnsets(scala.collection.JavaConversions.asJavaList(onsets.asInstanceOf[Seq[java.lang.Double]]))

    val tracks = PartialTracking.partialTracking(spectrogram.spectra)
    visualzer.drawPartialTracks(scala.collection.JavaConversions.asJavaList(tracks.toSeq))
    readLine()
  } */

  /*test("estimate tempo") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    val wave = new WaveFileReader(resource)

    val energyFlux = EnergyFluxComputation.computeEnergyFlux(wave.toTimeDomainWaveForm)
    val fluxSpectrogram = TempoEstimation.fluxSpectrogram(energyFlux)


    for (i <- 0 until fluxSpectrogram.spectra.size) {
      new SpectrumShower(null, i.toString, i, fluxSpectrogram.bandWidth,
        scala.collection.JavaConversions.asJavaList(fluxSpectrogram.spectra(i).amplitudes.toSeq.asInstanceOf[Seq[java.lang.Double]]))
    }

    readLine()
  }  */

 test("compute the tempo") {
    val resource: InputStream = getClass().getClassLoader().getResourceAsStream("dMajorScaleRecorder.wav");
    val wave = new WaveFileReader(resource)
    //val wave = new WaveFileReader("C:\\MFF\\rozehra\\tempo_train_data\\train3.wav")
    val spectrogram = wave.toTimeDomainWaveForm.segmentToWindows(1024, 512).toSpectrogram

    val visualzer = new Visualizer()
    visualzer.drawSpectrum(spectrogram)

    val (tempo: Tempo, onsets: Seq[Time]) =  cz.rozehra.signalProcessing.tempo.rhythmicAnalysis(wave.toTimeDomainWaveForm)
    visualzer.drawNoteOnsets(scala.collection.JavaConversions.asJavaList(onsets.asInstanceOf[Seq[java.lang.Double]]))

    println(tempo)
    visualzer.drawTempo(tempo)

    //val tracks = PartialTracking.partialTracking(spectrogram.spectra.map())
    //visualzer.drawPartialTracks(scala.collection.JavaConversions.asJavaList(tracks.toSeq))
    readLine()
  }
}
