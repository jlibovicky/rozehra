package cz.rozehra.signalProcessing.fft

case class Complex(re: Double, im: Double = 0.0) {
  def +(x: Complex): Complex = Complex((this.re+x.re), (this.im+x.im))
  def -(x: Complex): Complex = Complex((this.re-x.re), (this.im-x.im))
  def *(x: Complex): Complex = Complex(this.re*x.re-this.im*x.im, this.re*x.im+this.im*x.re)
}