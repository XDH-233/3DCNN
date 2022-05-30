package winograd

import breeze.linalg._
import breeze.numerics._

import scala.language.implicitConversions
import scala.reflect.ClassTag
import scala.{specialized => spec}

object DenseMatrixEx {
  implicit class DenseMatrixIntExtension(m: DenseMatrix[Int]) {
    def toDouble = {
      345
      val c = m.cols
      val r = m.rows
      DenseMatrix.tabulate(r, c)((i, j) => m(i, j).toDouble)
    }
    def slice(rs: Int = 0, re: Int, cs: Int = 0, ce: Int) = DenseMatrix.tabulate(re - rs + 1, ce - cs + 1)((i, j) => m(i + rs, j + cs))
  }

  implicit class DenseMatrixDoubleExtension(m: DenseMatrix[Double]) {
    def toInt = {
      val c = m.cols
      val r = m.rows
      DenseMatrix.tabulate(r, c)((i, j) => m(i, j).toInt)
    }
  }
}
