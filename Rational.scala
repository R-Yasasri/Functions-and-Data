package functions_and_data

object q2 extends App {

  class Rational(x: Int, y: Int) {

    def numer = x;

    def denom = y;

    def neg() = new Rational(-this.numer, this.denom)

    def +(r: Rational) = new Rational(this.numer *
      r.denom + r.numer * this.denom, denom * r.denom)

    def -(r: Rational) = this + r.neg
  }

  def ans() = {

    var temp = new Rational(3, 4) - new Rational(5, 8) - new Rational(2, 7)
    println("3/4 - 5/8 - 2/7 = "+temp.numer + "/" + temp.denom);

  }
ans();

}