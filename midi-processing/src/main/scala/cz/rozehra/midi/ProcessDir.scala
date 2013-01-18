package cz.rozehra.midi

import java.io.{PrintWriter, File}
import weka.core.{Instances, Attribute, FastVector}

object ProcessDir {
  def processFiles(directoryPath: String) = {
    val dir = new File(directoryPath)
    val arffDataSheet = emptyArff
    assert(dir.isDirectory)
    //val midiFiles = dir.listFiles.filter(_.getName.matches(".*\\.MID$"))
    val midiFiles = dir.listFiles
    val tracks = midiFiles.toSeq.par.map(FeatureExtractor.parseMidiFile(_, arffDataSheet)).seq.flatten
    (tracks, arffDataSheet)

    val writer = new PrintWriter(new File("C:\\MFF\\rozehra\\test.arff" ))
    writer.write(arffDataSheet.toString)
    writer.close()
  }

  def main(args: Array[String]) = {
    val dirPath = "C:\\MFF\\rozehra\\midi_test"
    processFiles(dirPath)
  }

  val fvMelodyVal = new FastVector(2)
  fvMelodyVal.addElement("YES")
  fvMelodyVal.addElement("NO")

  def emptyArff = {
    val atts = new FastVector()

    atts.addElement(new Attribute("melody", fvMelodyVal))
    atts.addElement(new Attribute("normalizedDuration"))
    atts.addElement(new Attribute("numberOfNotes"))
    atts.addElement(new Attribute("occupationRate"))
    atts.addElement(new Attribute("polyphonyRate"))
    atts.addElement(new Attribute("highestPith"))
    atts.addElement(new Attribute("normHighestPitch"))
    atts.addElement(new Attribute("lowestPitch"))
    atts.addElement(new Attribute("normLowestPitch"))
    atts.addElement(new Attribute("pitchMean"))
    atts.addElement(new Attribute("normPitchMean"))
    atts.addElement(new Attribute("pitchDeviation"))
    atts.addElement(new Attribute("normPitchDeviation"))
    atts.addElement(new Attribute("numberOfDifferentIntervals"))
    atts.addElement(new Attribute("normNumberOfDifferentIntervals"))
    atts.addElement(new Attribute("largestInterval"))
    atts.addElement(new Attribute("normLargestInterval"))
    atts.addElement(new Attribute("smallestInterval"))
    atts.addElement(new Attribute("normSmallestInterval"))
    atts.addElement(new Attribute("intervalMean"))
    atts.addElement(new Attribute("normIntervalMean"))
    atts.addElement(new Attribute("intervalMode"))
    atts.addElement(new Attribute("normIntervalMode"))
    atts.addElement(new Attribute("intervalDeviation"))
    atts.addElement(new Attribute("normIntervalDeviation"))
    atts.addElement(new Attribute("longestNote"))
    atts.addElement(new Attribute("normLongestNote"))
    atts.addElement(new Attribute("shortestNote"))
    atts.addElement(new Attribute("normShortestNote"))
    atts.addElement(new Attribute("durationMean"))
    atts.addElement(new Attribute("normDurationMean"))
    atts.addElement(new Attribute("durationDeviation"))
    atts.addElement(new Attribute("normDurationDeviation"))

   new Instances("MidiTracks", atts, 0)
  }
}
