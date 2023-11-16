import language.implicitConversions

case class Complex(real: Double, imaginary: Double = 0.0) {
  override def toString = s"$real $sign ${imaginary.abs}i"
  private def sign = if (imaginary >= 0.0) "+" else "-"
  def +(other: Complex) =
    Complex(real + other.real, imaginary + other.imaginary)
}
object Complex {
  implicit def intToComplex(i: Int): Complex = Complex(i)
}

Complex(2.0, 3.0) + 10

10 + Complex(2.0, 3.0)

class TimesInt(i: Int) {
  def times(fn: => Unit): Unit = {
    var x = 0
    while (x < i) {
      x += 1
      fn
    }
  }
}

implicit def intToTimesInt(i: Int): TimesInt = new TimesInt(i)

5 times { println("hello") }

intToTimesInt(5).times { println("hello") }