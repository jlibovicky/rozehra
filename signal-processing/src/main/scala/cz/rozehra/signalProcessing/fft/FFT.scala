package cz.rozehra.signalProcessing.fft

import scala.math._
import cz.rozehra.signalProcessing.Window
import collection.{mutable, immutable}

object FFT {
  def powerSpectrum[T <: Double](input:IndexedSeq[T]) = {
    val data = padder(input.map(i => Complex(i)).toList)
    val outComplex = fft(data)
    val out = outComplex.map(c => math.sqrt((c.re * c.re) + (c.im * c.im))).take((data.length / 2) + 1).toIndexedSeq
    out.map(i => (i / sqrt(data.length)).asInstanceOf[T]) // Power Spectral Density Output
  }

  def hammingWindow[T <: Double](original: Window[T]): immutable.IndexedSeq[T] = {
    val N = original.size
    val newWindowVector: mutable.IndexedSeq[T] = mutable.IndexedSeq.fill(N)(0.0.asInstanceOf[T])

    for (i <- 0 until original.size) {
      newWindowVector(i) = (0.5 * (1.0 - math.cos(2.0 * math.Pi * i / (N - 1.0))) * original.samples(i)).asInstanceOf[T]
    }

    newWindowVector.toIndexedSeq[T]
  }

  private def padder(data:List[Complex]) : List[Complex] = {
    def check(num:Int) : Boolean = if((num.&(num-1)) == 0) true else false
    def pad(i:Int) : Int = {
      check(i) match {
        case true => i
        case false => pad(i + 1)
      }
    }         
    if(check(data.length) == true) data else data.padTo(pad(data.length), Complex(0))
  } 

  def fft(f: List[Complex]) : List[Complex] = {
    require(isPowerOf2(f.size), "Input of FFT must have size of power of two.")
    f.size match {
      case 0 => Nil
      case 1 => f
      case n => {
        val c: Double => Complex = phi => Complex(cos(phi), sin(phi))
        val e = fft(f.zipWithIndex.filter(_._2%2==0).map(_._1))
        val o  = fft(f.zipWithIndex.filter(_._2%2!=0).map(_._1))
        def it(in:List[(Int, Complex)], k:Int = 0) : List[(Int, Complex)] = {
          k < (n / 2) match {
            case true => it( (k+n/2,e(k)-o(k)*c(-2*Pi*k/n)) :: (k,e(k)+o(k)*c(-2*Pi*k/n)) :: in, k + 1)
            case false => in
          } 
        }
        it(List[(Int, Complex)]()).sortWith((x,y) => x._1 < y._1).map(_._2)
      }
    }
  }

  private def isPowerOf2(n: Int): Boolean =
   if (n == 2 || n == 1) true
   else if (n % 2 == 1) false
        else isPowerOf2(n / 2)
}