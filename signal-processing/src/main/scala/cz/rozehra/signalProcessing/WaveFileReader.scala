package cz.rozehra.signalProcessing

import java.io._
import scala.collection.JavaConversions._
import scala.collection.mutable.ListBuffer

class WaveFileReader(val input: InputStream) { 
  def this(path: String) = this(new FileInputStream(path))
  val stream = new DataInputStream(input)
  
  // METHODS FOR READING FROM THE FILE
  
  private def readFourChars(): String =
   stream.readByte().toChar.toString + stream.readByte().toChar.toString + 
     stream.readByte().toChar.toString + stream.readByte().toChar.toString 
  
  private def readFourBytesUInt32(): Long =
    (stream.readByte & 0xff).toLong + 256 * (stream.readByte & 0xff).toLong + 
      256 * 256 * (stream.readByte & 0xff).toLong + 256 * 256 * 256 * (stream.readByte & 0xff).toLong
    
  private def readTwoBytesUInt16(): Long =
    (stream.readByte & 0xff).toLong  + 256 * (stream.readByte & 0xff).toLong
    
  private def readNumOfNBytes(N: Int): Double = {
    def readNumOfNBytes0(i: Int, N: Int, accu: Double): Double =
      if (i >= N) accu
      else {
        val readByte = stream.readByte & 0xff
        readNumOfNBytes0(i + 1, N, Math.pow(256, i) + accu)
      }
    readNumOfNBytes0(0, N, 0)
  }     
        
  private def readSample8(): Double = stream.readByte().asInstanceOf[Double] / Byte.MaxValue.asInstanceOf[Double]
  private def readSample16(): Double = java.lang.Short.reverseBytes(stream.readShort()).asInstanceOf[Double] / Short.MaxValue.asInstanceOf[Double]
  private def readSample32(): Double = java.lang.Integer.reverseBytes(stream.readInt()).asInstanceOf[Double] / Int.MaxValue.asInstanceOf[Double]
  private def readSample64(): Double = java.lang.Long.reverseBytes(stream.readLong()).asInstanceOf[Double] / Long.MaxValue.asInstanceOf[Double]
         
  // IN FACT CONSTRUCTOR OF THE WAVE FILE
       
  if(readFourChars != "RIFF") { stream.close; throw new RuntimeException("Wrong format of file - not a RIFF file") }
  
  private val RIFF: Long = readFourBytesUInt32
  
  if(readFourChars != "WAVE") { stream.close; throw new RuntimeException("Wrong format of file - not a WAVE file") }
  
  while (readFourChars != "fmt ") {
    // skip this chunk's data field
    val otherChunkLen = readFourBytesUInt32;
    for (i <- 0 until otherChunkLen.asInstanceOf[Int]) {
      stream.readByte()
    }
  }
  
  val fmtSize = readFourBytesUInt32
  if (fmtSize < 16) throw new RuntimeException("Wrong format of file - fmt subchunk length.")
  
  val fmtTag = readTwoBytesUInt16
  if (fmtTag != 1) throw new RuntimeException("Unknown type of Wave file compression.") 
  
  val channels = readTwoBytesUInt16
  val samplesPerSec = readFourBytesUInt32
  val samplingRate: Frequency = samplesPerSec.toDouble
  val averageBytesPerSec = readFourBytesUInt32
  val blockAlign = readTwoBytesUInt16
  val bitsPerSample = readTwoBytesUInt16
  
  for (i <- 16 until fmtSize.asInstanceOf[Int]) stream.readByte()
  while (readFourChars != "data") { val dummy = false }
  val dataLength = readFourBytesUInt32 

  def readData: IndexedSeq[Signal] = {
    var accu: List[Signal] = Nil
    var alreadyReadCount = 0l
    try {
      while (alreadyReadCount < dataLength) {
        alreadyReadCount += 1
        var nextSample: Double = 0        
        bitsPerSample match {
          case 8 => nextSample = readSample8
          case 16 => nextSample = readSample16
          case 32 => nextSample = readSample32
          case 64 => nextSample = readSample64
        }
        accu ::= nextSample
      }
      accu.reverse.toIndexedSeq
    }
    catch {
      case e: IOException => accu.reverse.toIndexedSeq
    } 
  }
  
  val data: IndexedSeq[Signal] = readData
  def getData: java.util.List[Signal] = ListBuffer(data: _*)
      
  stream.close
  
  def segmentToWindows(windowLength: Int, windowShift: Int): WindowedTimeDomain[Signal] = {
    val windowsDataMain = data.sliding(windowLength, windowShift).toSeq

    val restSamplesOnRight = data.size - (data.size / windowShift) * windowShift
    val windowsData = if (restSamplesOnRight > 0) windowsDataMain :+ data.takeRight(restSamplesOnRight)
                      else windowsDataMain
    val windows = windowsData.map( s => new Window[Signal](samplingRate, windowShift, s.toIndexedSeq, windowLength))

    new WindowedTimeDomain(samplingRate, windowLength, windowShift, windows.toList)
  }
  
  override def toString: String = {
    "Wave File\n" + "  sampling rate: " + samplesPerSec + " Hz\n" +
     "  bits per sample: " + bitsPerSample + "\n" +
     "  declared number of samples: " + dataLength + "\n" +
     "  real number of sample: " + data.length 
  }
  
  def plot: Unit = {
    /*Plotting.plot(DenseVector.range(0, data.length) / samplingRate, new DenseVectorRow(data.toArray))
    Plotting.title("Wave file")
    Plotting.xlabel("Time in seconds")*/
  }
  
  def plot(fileName: String): Unit = {
    /*plot
    Plotting.saveas(fileName)*/
  }

  def toTimeDomainWaveForm = new TimeDomainWaveForm[Signal](samplingRate, data.toIndexedSeq)
}