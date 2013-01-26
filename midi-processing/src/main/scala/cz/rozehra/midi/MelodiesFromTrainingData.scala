package cz.rozehra.midi

import java.io.{FileOutputStream, ObjectOutputStream, File}

object MelodiesFromTrainingData {
   def main(args: Array[String]) = {
     val name = args(0)

     val directory = new File(args(1))

     val oos = new ObjectOutputStream(new FileOutputStream(name + ".dat"))
     var melodies = 0
     for (midiFile <- directory.listFiles()) {
       for (melody <- new FeatureExtractor(midiFile).tracks.filter(_.melody)
         .map(TrackPostprocessing.takeMelodiesFromTrack(_)).flatten) {
         oos.writeObject(melody)
         melodies += 1
       }
     }

     oos.close()

     println(melodies + " snippets in " + directory.listFiles().size + " files")
   }
}
