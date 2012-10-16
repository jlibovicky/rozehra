package cz.rozehra.signalProcessing

class Window[T](val samplingRate: Frequency, val samples: List[T]) {
    def size = samples.length
}