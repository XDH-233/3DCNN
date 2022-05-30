package winograd

object NumberTheory {
  def GCD(a: Int, b: Int): Int = {
    if (b == 0) a
    else GCD(b, a % b)
  }

  //[gcd, x, y] = exGCD(a, b) => ax + by = gcd(a, b)
  def exGCD(a: Int, b: Int): Seq[Int] = {
    if (b == 0) Seq(a, 1, 0)
    else {
      val res = exGCD(b, a % b)
      val X   = res(1)
      val Y   = res(2)
      Seq(res.head, Y, X - (a / b) * Y)
    }
  }

  def crt(A: Seq[Int], M: Seq[Int]) = {
    require(A.length == M.length)
    val N = M.product
    A.zip(M).map { case (a, m) => a * (N / m) * exGCD(N / m, m)(1) }.sum % N
  }
}
