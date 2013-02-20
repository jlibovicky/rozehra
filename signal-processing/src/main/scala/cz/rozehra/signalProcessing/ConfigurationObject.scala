package cz.rozehra.signalProcessing

import java.io.FileInputStream


object ConfigurationObject {
  val configuration = new Configuration(new FileInputStream("config.xml"))
}
