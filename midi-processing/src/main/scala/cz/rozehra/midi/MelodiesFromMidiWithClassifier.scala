package cz.rozehra.midi

import java.io._
import scala.io.Source

object MelodiesFromMidiWithClassifier {
  val classifier = WekaUtils.classifier

  def getMelodiesFromTrack(track: TrackVector): Seq[Melody] = {
    val score = classifier.distributionForInstance(track.getWekaInstanceWithDataSet)

    if (score(0) > 0.5) TrackPostprocessing.takeMelodiesFromTrack(track)
    else Seq.empty
  }

  def main(args: Array[String]) {
    val name = args(0)
    val directory = args(1)
    val fileWithFiles = args(2)

    val oos = new ObjectOutputStream(new FileOutputStream(name + ".dat"))

    var melodies = 0
    var totalFiles = 0
    var usedFiles = 0

    for(fileName <- Source.fromFile(fileWithFiles).getLines()
        if new File(directory + "\\" + fileName).exists()) {
      val midiFilePath = directory + "\\" + fileName
      val midiFile = new File(midiFilePath)
      var fileUsed = false

      for (melody <- new FeatureExtractor(midiFile).tracks.map(getMelodiesFromTrack(_)).flatten) {
        oos.writeObject(melody)
        fileUsed = true
        melodies += 1
      }

      if (fileUsed) usedFiles += 1
      totalFiles += 1
    }

    oos.flush()
    oos.close()

    println("melody detected in " + usedFiles + " files from " + totalFiles)
    println("totally " + melodies + " snippets")
  }
}
