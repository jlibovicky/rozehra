package cz.rozehra.midi

import java.io._
import scala.util.Random


object GenerateSRILMdata {
  def main(args: Array[String]) {
      for (dataFile <- args.map(new File(_))) {
        if (!dataFile.exists()) { System.err.println(dataFile.getAbsolutePath + " does note exist.") }
        else {
          val style = dataFile.getName.replaceFirst("\\.dat$", "")
          val ois = new ObjectInputStream(new FileInputStream(dataFile))
          var melodies = Seq.empty[Melody]

          var finished = false
          try {
            while (!finished) {
              val obj = ois.readObject()
              if (obj == null) finished = true
              else melodies :+= obj.asInstanceOf[Melody]
            }
          } catch {
            case e:IOException => Unit
          }

          ois.close()

          val randomlyOrdered = Random.shuffle(melodies)
          val (testSet, trainSet) = randomlyOrdered.splitAt((0.1 * randomlyOrdered.size).asInstanceOf[Int])

          // 1. create ratio data
          val ratioTrainingFile = new BufferedWriter(new FileWriter(style + "-rat.train"))
          val ratioTestFile = new BufferedWriter(new FileWriter(style + "-rat.test"))

          trainSet.map( m => ratioTrainingFile.write(m.getLMNotation(LMFormat.Round1Rat)) )
          testSet.map( m => ratioTestFile.write(m.getLMNotation(LMFormat.Round1Rat)) )

          // 2. create the log - round 1 data

          val log1TrainingFile = new BufferedWriter(new FileWriter(style + "-log1.train"))
          val log1TestFile = new BufferedWriter(new FileWriter(style + "-log1.test"))

          trainSet.map( m => log1TrainingFile.write(m.getLMNotation(LMFormat.Round1Log)) )
          testSet.map( m => log1TestFile.write(m.getLMNotation(LMFormat.Round1Log)) )

          // 3. create the log - round 2 data

          val log2TrainingFile = new BufferedWriter(new FileWriter(style + "-log2.train"))
          val log2ioTestFile = new BufferedWriter(new FileWriter(style + "-log2.test"))

          trainSet.map( m => log2TrainingFile.write(m.getLMNotation(LMFormat.Round2Log)) )
          testSet.map( m => log2ioTestFile.write(m.getLMNotation(LMFormat.Round2Log)) )
        }
      }
    }
}
