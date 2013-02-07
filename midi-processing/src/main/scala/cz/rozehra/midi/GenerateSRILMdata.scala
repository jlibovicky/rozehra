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

          // 1. create ratio data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
          val ratio0TrainingFile = new BufferedWriter(new FileWriter(style + "-rat0.train"))
          val ratio0TestFile = new BufferedWriter(new FileWriter(style + "-rat0.test"))

          trainSet.map( m => ratio0TrainingFile.write(m.getLMNotation(LMFormat.Round0Rat)) )
          testSet.map( m => ratio0TestFile.write(m.getLMNotation(LMFormat.Round0Rat)) )

          ratio0TrainingFile.close()
          ratio0TestFile.close()

          // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          val ratio1TrainingFile = new BufferedWriter(new FileWriter(style + "-rat1.train"))
          val ratio1TestFile = new BufferedWriter(new FileWriter(style + "-rat1.test"))

          trainSet.map( m => ratio1TrainingFile.write(m.getLMNotation(LMFormat.Round1Rat)) )
          testSet.map( m => ratio1TestFile.write(m.getLMNotation(LMFormat.Round1Rat)) )

          // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          val ratio2TrainingFile = new BufferedWriter(new FileWriter(style + "-rat2.train"))
          val ratio2TestFile = new BufferedWriter(new FileWriter(style + "-rat2.test"))

          trainSet.map( m => ratio2TrainingFile.write(m.getLMNotation(LMFormat.Round2Rat)) )
          testSet.map( m => ratio2TestFile.write(m.getLMNotation(LMFormat.Round2Rat)) )

          // 2. create the log data - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          val log1TrainingFile = new BufferedWriter(new FileWriter(style + "-log1.train"))
          val log1TestFile = new BufferedWriter(new FileWriter(style + "-log1.test"))

          trainSet.map( m => log1TrainingFile.write(m.getLMNotation(LMFormat.Round1Log)) )
          testSet.map( m => log1TestFile.write(m.getLMNotation(LMFormat.Round1Log)) )

          // - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

          val log2TrainingFile = new BufferedWriter(new FileWriter(style + "-log2.train"))
          val log2ioTestFile = new BufferedWriter(new FileWriter(style + "-log2.test"))

          trainSet.map( m => log2TrainingFile.write(m.getLMNotation(LMFormat.Round2Log)) )
          testSet.map( m => log2ioTestFile.write(m.getLMNotation(LMFormat.Round2Log)) )
        }
      }
    }
}
