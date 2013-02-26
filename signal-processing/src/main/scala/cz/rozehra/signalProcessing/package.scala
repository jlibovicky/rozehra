package cz.rozehra

import scala.collection.immutable.IndexedSeq
import org.apache.commons.math3.linear.RealMatrix

package object signalProcessing {
  type Time = Double
  type Signal = Double
  type Frequency = Double
  type Energy = Double
  type EnergyFlux = Double
    
    def sumSequences[T <: Double](seq1: IndexedSeq[T], seq2: IndexedSeq[T]): IndexedSeq[T] = {
          def sumSamples0(seq1: IndexedSeq[T], seq2: IndexedSeq[T], accu: List[T]): IndexedSeq[T] =
            if (seq1.isEmpty) accu.reverse.toIndexedSeq
            else {
              sumSamples0(seq1.tail, seq2.tail, (seq1.head + seq2.head).asInstanceOf[T] :: accu)
            }
        sumSamples0(seq1, seq2, Nil)          
    }

  def matrixToSpectra[T <: Double](matrix: RealMatrix, withWindowShift: Int, bandWidth: Frequency) = {
    var spectraRev = List.empty[Spectrum[T]]
    for (i <- 0 until matrix.getColumnDimension)
      spectraRev ::= new Spectrum[T](withWindowShift, bandWidth, matrix.getColumn(i).map(_.asInstanceOf[T]).toIndexedSeq)
    spectraRev.reverse
  }
}