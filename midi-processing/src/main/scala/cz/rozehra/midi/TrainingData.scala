package cz.rozehra.midi

import java.io.{PrintWriter, File}
import weka.core.{Instances, Attribute, FastVector}

object TrainingData {
  def processFiles(directoryPath: String) = {
    val dir = new File(directoryPath)
    val arffDataSheet = new Instances("MidiTracks", WekaUtils.wekaAttributes, 0)
    assert(dir.isDirectory)
    val midiFiles = dir.listFiles
    val tracks = midiFiles.toSeq.map(new FeatureExtractor(_)).map(_.tracks).flatten
    for (track <- tracks) arffDataSheet.add(track.getWekaInstance)

    val writer = new PrintWriter(new File("C:\\MFF\\rozehra\\train.arff" ))
    writer.write(arffDataSheet.toString)
    writer.close()
  }

  def main(args: Array[String]) = {
    val dirPath = "C:\\MFF\\rozehra\\midi_test"
    processFiles(dirPath)
  }
}
