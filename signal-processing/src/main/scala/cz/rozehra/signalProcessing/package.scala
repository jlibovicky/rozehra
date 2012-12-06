package cz.rozehra

import scala.collection.immutable.IndexedSeq

package object signalProcessing {
  type Time = Double
  type Signal = Double
  type Frequency = Double
  type Energy = Double
  type EnergyFlux = Double
    
    def sumSequences[T <: Double](seq1: IndexedSeq[T], seq2: IndexedSeq[T]): IndexedSeq[T] = {
          def sumSamples0(seq1: IndexedSeq[T], seq2: IndexedSeq[T], accu: List[T]): IndexedSeq[T] =
            if (seq1.isEmpty) accu.reverse.toIndexedSeq[T]
            else {
              sumSamples0(seq1.tail, seq2.tail, (seq1.head + seq2.head).asInstanceOf[T] :: accu)
            }
        sumSamples0(seq1, seq2, Nil)          
    }
}