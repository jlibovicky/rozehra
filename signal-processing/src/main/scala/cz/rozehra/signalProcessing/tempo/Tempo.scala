package cz.rozehra.signalProcessing.tempo

import cz.rozehra.signalProcessing._;

class Tempo(val tempo: Frequency, val shift: Time) {
  override def toString: String = { "metrum " + tempo + " Hz, time shift " + shift }
}
