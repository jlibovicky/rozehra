package cz.rozehra.signalProcessing.tempo

import akka.actor._
import akka.routing.RoundRobinRouter
import scala.concurrent.util.duration._
import concurrent.util.Duration
import cz.rozehra.signalProcessing._
import collection.IndexedSeq
import math._

object EnergyFluxComputation extends App {
  // ENERGY FLUX COMPUTATION PARAMETERS
  val alpha = 2.0
  val beta = 2.0
  val T1: Time = 0.1
  val T2: Time = 0.7
  val energyWindowSize = 64 // this is in fact two time more
  val energyWindowShift = 32
  val bandWidth: Frequency = 360//22050//360

  sealed trait BandMessage
  case object Compute extends BandMessage
  case class Work(band: TimeDomainWaveForm[Signal]) extends BandMessage
  case class Result(value: IndexedSeq[EnergyFlux]) extends BandMessage
  case class FluxComputation(bandsProcessed: Int, bandsTotal: Int, duration: Duration)

  // A worker -- class capable of processing one particular signal band
  class Worker extends Actor {

    def getBandEnergyFlux(band: TimeDomainWaveForm[Signal]): IndexedSeq[EnergyFlux] = {
      val bandEnergy = band.getEnergy(64, 32)

      val bandEnergyDerivative = new TimeDomainWaveForm[EnergyFlux](bandEnergy.samplingRate,
        Filters.simpleDerivative(bandEnergy.samples, bandEnergy.samplingRate))

      val filteredFlux = Filters.lowPassFilter[EnergyFlux](bandEnergyDerivative.samples,
        bandEnergyDerivative.samplingRate, 10)

      filteredFlux
    }

    def receive = {
      case Work(band) ⇒
        sender ! Result(getBandEnergyFlux(band)) // perform the work
    }
  }

  class Master(nrOfWorkers: Int, signal: TimeDomainWaveForm[Signal], listener: ActorRef)
    extends Actor {

    def sumSeqs(seq1: collection.mutable.IndexedSeq[EnergyFlux], seq2: IndexedSeq[EnergyFlux]) =
      for (i <- 0 until min(seq1.length, seq2.length)) seq1(i) += seq2(i)

    val bandCount = round(signal.samplingRate / bandWidth).asInstanceOf[Int] // number of band the signal going split to
    val resultFlux = collection.mutable.IndexedSeq.fill[Signal](signal.samples.length / energyWindowShift)(0.0)
    val start: Long = System.currentTimeMillis
    var processedBandsCount = 0

    val workerRouter = context.actorOf(
      Props[Worker].withRouter(RoundRobinRouter(nrOfWorkers)), name = "workerRouter")

    def receive = {
      case Compute ⇒
        for (i <- 0 until bandCount) {
          val iThBandSignalSamples = Filters.bandPassFilter(signal.samples,
            signal.samplingRate, i * bandWidth, (i + 1) * bandWidth)
          val iThBandSignal = new TimeDomainWaveForm[Signal](signal.samplingRate, iThBandSignalSamples)
          workerRouter ! Work(iThBandSignal)
        }

      case Result(bandFlux) ⇒
        sumSeqs(resultFlux, bandFlux)
        processedBandsCount += 1
        if (processedBandsCount == bandCount) {
          // Send the result to the listener
          listener ! FluxComputation(processedBandsCount, bandCount, duration = (System.currentTimeMillis - start).millis)
          // Stops this actor and all its supervised children
          context.stop(self)
        }
    }

  }

  class Listener extends Actor {
    def receive = {
      case FluxComputation(bandsProcessed, bandCount, duration) ⇒
        println("\n\tEnergy flux computation: \t\t%s of %s\n\tCalculation time: \t%s"
          .format(bandsProcessed, bandCount, duration))
        context.system.shutdown()
    }
  }


  def calculate(nrOfWorkers: Int, signal: TimeDomainWaveForm[Signal]) =  {
    // Create an Akka system
    val system = ActorSystem("EnergyFluxSystem")

    // create the result listener, which will print the result and shutdown the system
    val listener = system.actorOf(Props[Listener], name = "listener")

    // create the master
    val masterObject = new Master(nrOfWorkers, signal, listener)
    val master = system.actorOf(Props(masterObject), name = "master")

    // start the calculation
    master ! Compute
    masterObject.resultFlux
  }

  def computeEnergyFlux(signal: TimeDomainWaveForm[Signal]): TimeDomainWaveForm[EnergyFlux] = {
    val resultSignal = calculate(4, signal).toIndexedSeq
    new TimeDomainWaveForm[EnergyFlux](signal.samplingRate / energyWindowShift, resultSignal)
  }
}