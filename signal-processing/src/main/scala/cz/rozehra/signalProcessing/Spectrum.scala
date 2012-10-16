package cz.rozehra.signalProcessing
import fft.FFT

class Spectrum[T <: Double](val duration: Time, val bandWidth: Frequency, val amplitudes: List[T]) { 
    def maxFrequency = bandWidth * amplitudes.length
}