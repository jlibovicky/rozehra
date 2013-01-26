package cz.rozehra.midi

import weka.core.{Instances, Attribute, FastVector}
import weka.classifiers.Classifier
import weka.classifiers.trees.RandomForest
import java.io.{InputStreamReader, BufferedReader, ObjectInputStream}
import weka.core.converters.ArffLoader.ArffReader

object WekaUtils {
  lazy val fvMelodyVal = {
    val melodyVal = new FastVector(2)
    melodyVal.addElement("YES")
    melodyVal.addElement("NO")
    melodyVal
  }

  lazy val wekaAttributes = {
    val attributes = new FastVector()
    attributes.addElement(new Attribute("melody", fvMelodyVal))
    attributes.addElement(new Attribute("normalizedDuration"))
    attributes.addElement(new Attribute("numberOfNotes"))
    attributes.addElement(new Attribute("occupationRate"))
    attributes.addElement(new Attribute("polyphonyRate"))
    attributes.addElement(new Attribute("highestPith"))
    attributes.addElement(new Attribute("normHighestPitch"))
    attributes.addElement(new Attribute("lowestPitch"))
    attributes.addElement(new Attribute("normLowestPitch"))
    attributes.addElement(new Attribute("pitchMean"))
    attributes.addElement(new Attribute("normPitchMean"))
    attributes.addElement(new Attribute("pitchDeviation"))
    attributes.addElement(new Attribute("normPitchDeviation"))
    attributes.addElement(new Attribute("numberOfDifferentIntervals"))
    attributes.addElement(new Attribute("normNumberOfDifferentIntervals"))
    attributes.addElement(new Attribute("largestInterval"))
    attributes.addElement(new Attribute("normLargestInterval"))
    attributes.addElement(new Attribute("smallestInterval"))
    attributes.addElement(new Attribute("normSmallestInterval"))
    attributes.addElement(new Attribute("intervalMean"))
    attributes.addElement(new Attribute("normIntervalMean"))
    attributes.addElement(new Attribute("intervalMode"))
    attributes.addElement(new Attribute("normIntervalMode"))
    attributes.addElement(new Attribute("intervalDeviation"))
    attributes.addElement(new Attribute("normIntervalDeviation"))
    attributes.addElement(new Attribute("longestNote"))
    attributes.addElement(new Attribute("normLongestNote"))
    attributes.addElement(new Attribute("shortestNote"))
    attributes.addElement(new Attribute("normShortestNote"))
    attributes.addElement(new Attribute("durationMean"))
    attributes.addElement(new Attribute("normDurationMean"))
    attributes.addElement(new Attribute("durationDeviation"))
    attributes.addElement(new Attribute("normDurationDeviation"))
    attributes
  }

  lazy val dataset = {
    // load training set
    val is = new InputStreamReader(getClass.getResourceAsStream("/train.arff"))
    val arff = new ArffReader(new BufferedReader(is))
    val data = arff.getData
    data.setClassIndex(0)
    data
  }

  lazy val classifier = {
    val classifier : Classifier = new RandomForest()

    // train the classifier
    classifier.buildClassifier(dataset)
    classifier
  }
}
