package winograd
import breeze.linalg._
import breeze.numerics._
import DenseMatrixEx._

object winCNN {
  def F(a: Seq[Int], n: Int) = {
    DenseMatrix.tabulate(n, 1)((i, j) => a.zipWithIndex.map { case (v, k) => if (k == i) 1 else a(i) - v }.product)
  }

  def FDiag(a: Seq[Int], n: Int) = {
    val f = F(a, n)
    DenseMatrix.tabulate(n, n)((i, j) => if (i == j) f(i, 0) else 0)
  }

  def FDiagPlus1(a: Seq[Int], n: Int) = {
    val f          = FDiag(a, n - 1)
    val rightZeros = DenseMatrix.zeros[Int](n - 1, 1)
    val lastRow    = DenseMatrix.tabulate(1, n)((i, j) => if (j == n - 1) 1 else 0)
    DenseMatrix.vertcat(DenseMatrix.horzcat(f, rightZeros), lastRow)
  }

  def At(a: Seq[Int], m: Int, n: Int) = {
    DenseMatrix.tabulate(m, n)((i, j) => BigInt(a(i)).pow(j).toInt)
  }

  def A(a: Seq[Int], m: Int, n: Int) = {
    val at      = At(a, m - 1, n)
    val lastRow = DenseMatrix.tabulate(1, n)((i, j) => if (j == n - 1) 1 else 0)
    DenseMatrix.vertcat(at, lastRow)
  }

  def Lx(a: Seq[Int], n: Int): DenseMatrix[Poly] =
    DenseMatrix.tabulate(n, 1)((i, j) => a.zipWithIndex.map { case (v, k) => if (k == i) Poly(Seq(1)) else Poly(Seq(-v, 1)) }.reduce(_ * _))

  def L(a: Seq[Int], n: Int): DenseMatrix[Double] = {
    val lx = Lx(a, n)
    val f  = F(a, n)
    DenseMatrix.tabulate(n, n)((i, j) => (lx(i, 0).nth(j).toDouble) / (f(i, 0).toDouble)).t
  }

  def T(a: Seq[Int], n: Int) = {
    DenseMatrix.horzcat(DenseMatrix.eye[Int](n), DenseMatrix.tabulate(n, 1)((i, j) => BigInt(-a(i)).pow(n).toInt))
  }

  def Bt(a: Seq[Int], n: Int) = L(a, n) * (T(a, n).toDouble)

  def B(a: Seq[Int], n: Int) = {
    DenseMatrix.vertcat(Bt(a, n - 1), DenseMatrix.tabulate(1, n)((i, j) => if (j == n - 1) 1.0 else 0.0))
  }

  def AT(a: Seq[Int], n: Int, r: Int) = {
    val alpha = n + r - 1
    A(a, alpha, n).t
  }

  def G(a: Seq[Int], n: Int, r: Int) = {
    val alpha = n + r - 1
    val f     = FDiagPlus1(a, alpha)
    (A(a, alpha, r).t.toDouble * inv(f)).t
  }

  def BT(a: Seq[Int], n: Int, r: Int) = {
    val alpha = n + r - 1
    val f     = FDiagPlus1(a, alpha)
    (f.toDouble * (B(a, alpha).t)).toInt
  }

  def conv2D(x: DenseMatrix[Int], h: DenseMatrix[Int]) = { // TODO more matrix size and more step
    val n     = x.rows
    val r     = h.rows
    val steps = n - r + 1
    val res = DenseMatrix.tabulate(steps, steps) { (i, j) =>
      var sub = x.slice(i, i + r - 1, j, j + r - 1)
      sum((sub *:* h))
    }
    res
  }

  def polyCoff(n: Int, r: Int) = {
    Range(0, n + r - 2).map(i => scala.math.ceil(i.toDouble / 2).toInt).zipWithIndex.map { case (v, i) => if (i % 2 == 0) -v else v }.toSeq
  }

  def conv2DWin(x: DenseMatrix[Int], h: DenseMatrix[Int]) = {
    val n    = x.rows - h.rows + 1
    val r    = h.rows
    val a    = polyCoff(n, r)
    val at   = AT(a, n, r)
    val g    = G(a, n, r)
    val bt   = BT(a, n, r)
    val hCov = (g * (h.toDouble) * (g.t))
    val xCov = (bt * x * (bt.t)).toDouble
    (at.toDouble * (hCov *:* xCov) * ((at.t).toDouble)).toInt
  }
}
