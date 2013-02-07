package cz.rozehra.midi

import java.io.{IOException, FileInputStream, ObjectInputStream, File}

object MelodySnippetsLength {
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

        val lengthInMillis = melodies.foldLeft(0.0)(_ + _.getMelodyLength)
        println(style + "\t" + formatTime(lengthInMillis) + "\t" + melodies.size + " snippets")
      }
    }
  }

  private def formatTime(millis: Double): String = {
    val secondsInt = (millis / 1000).asInstanceOf[Int]
    val ss = secondsInt % 60
    val mm = (secondsInt / 60) % 60
    val hh = secondsInt / 3600

    hh.toString + ":" + "%02d".format(mm) + ":" + "%02d".format(ss)
  }
}
