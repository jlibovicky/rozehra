package cz.rozehra.signalProcessing

import languageModeling.{SRILMWrapperRescore, LMFormat}
import partialtracking._
import pitchDetection.harmonicSpectrumProduct.{CBHSP, HSP}
import pitchDetection.klapuriWhitening.KlapuriFundamentalDetection
import pitchDetection.{PeakDetection, CombinedFundamentalsDetection, FundamentalsDetection}
import trackSelection.{TrackSelectionParameters, TrackSelection}
import visualization.Visualizer
import collection.JavaConversions

object AME {
  var pitchDetection: FundamentalsDetection = KlapuriFundamentalDetection
  var trackingAlgorithm: GenericPartialTracking[Signal] = PartialTrackingForFundamentals
  var ngramCmd: String = "ngram"
  var wavFile: String = ""

  val pitch = """^-pitch=(.*)$""".r
  val model = """^-model=(.*)$""".r
  val ngramPath = """^-ngramPath=(.*)$""".r
  val fileReg = """^([^-].*)$""".r
  val srilmPort = """^-srilmPort=(.*)$""".r

  def main(args: Array[String]) {
    for (arg <- args) {
      arg match {
        case pitch(method) => method match {
          case "peaks" => pitchDetection = PeakDetection; trackingAlgorithm = PartialTrackingForPeaks
          case "hsp" => pitchDetection = HSP; trackingAlgorithm = PartialTrackingForHSP
          case "cbhsp" => pitchDetection = CBHSP; trackingAlgorithm = PartialTrackingForCBHSP
          case "salience" => pitchDetection = KlapuriFundamentalDetection; trackingAlgorithm = PartialTrackingForFundamentals
          case "combined" => pitchDetection = CombinedFundamentalsDetection; trackingAlgorithm = PartialTrackingForCombined
          case _ => System.err.println("Invalid pitch detection algorithm " + method + ". " +
            "Salience function maximization used instead.")
        }
        case model(modelType) => modelType match {
          case "plain" => TrackSelectionParameters.lmFormat = LMFormat.Round0Rat
          case "clust" => TrackSelectionParameters.lmFormat = LMFormat.Round0RatClust
          case _ => TrackSelectionParameters.lmFormat = LMFormat.None
        }
        case ngramPath(path) => ngramCmd = path
        case fileReg(fileName) => wavFile = fileName
        case srilmPort(port) => TrackSelectionParameters.srilmPort = port.toInt
        case _ => System.err.println("Unknown option " + arg + "."); System.exit(-1)
      }
    }

    if (TrackSelectionParameters.lmFormat != LMFormat.None) {
      TrackSelectionParameters.languageModel = new SRILMWrapperRescore(ngramCmd, TrackSelectionParameters.lmFormat)
    }

    val readFileStart = System.currentTimeMillis
    val wave = new WaveFileReader(wavFile)
    val readFileEnd = System.currentTimeMillis
    println("Reading file: " + (readFileEnd - readFileStart) / 1000.0 + " s")

    val spectrogram = wave.segmentToWindows(4096, 2048).toSpectrogram
    val spectrogramComputed = System.currentTimeMillis
    println("Spectrogram computed: " + (spectrogramComputed - readFileEnd) / 1000.0 + " s")

    val visualizer = new Visualizer()
    visualizer.drawSpectrum(spectrogram)

    //val solutions = loadCorrectSolution("C:\\MFF\\rozehra\\mirex05TrainFiles\\train01REF.txt")
    //visualizer.drawSolution(JavaConversions.asJavaList(solutions.map(_.asInstanceOf[java.lang.Double])))

    val spectrumRate = pitchDetection.spectrogramSamplingRate(wave.samplingRate)
    val fundamentalsStart = System.currentTimeMillis
    val detectedFundamentals = pitchDetection.detectFundamentals(wave.toTimeDomainWaveForm)
    val fundamentalsEnd = System.currentTimeMillis
    println("Fundamentals detection: " + (fundamentalsEnd - fundamentalsStart) / 1000.0 + " s")
    visualizer.drawFundamentals(JavaConversions.asJavaList((
      detectedFundamentals.map(_.map(_._1.asInstanceOf[java.lang.Double])))),
      spectrumRate)

    val tracks = trackingAlgorithm.partialTracking(detectedFundamentals)
    val trackingEnd = System.currentTimeMillis
    println("Partial tracking: " + (trackingEnd - fundamentalsEnd) / 1000.0 + " s")
    //tracks.foreach( t => println(t) )

    visualizer.drawPartialTracks(JavaConversions.asJavaList(tracks.toSeq), spectrumRate)

    val searchingStart = System.currentTimeMillis()
    val result = TrackSelection.run(TrackSelection.convertTrackToSearchTracks(tracks, spectrumRate))
    println("Track searching: " + (System.currentTimeMillis() - searchingStart) / 1000.0 + " s")

    if (result.nonEmpty) {
      visualizer.drawHypothesis(result.head)
    }
  }
}
