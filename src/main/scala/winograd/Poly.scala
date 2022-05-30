package winograd

case class Poly(coff: Seq[Int]) {
  def *(that: Poly) = Poly(conv(coff, that.coff))

  def toSting = {
    var str = ""
    coff.zipWithIndex.foreach { case (c, i) =>
      if (c != 0) {
        if (c > 0)
          str = str + "+"

        if (c == -1)
          str = str + "-"
        else if (c != 1)
          str = str + s"${c}"

        if (i == 1)
          str = str + "x"
        else
          str = str + s"x^${i}"
      }
    }
    if (str.head == '+')
      str = str.substring(1)
    str
  }

  def nth(i: Int) = coff(i)

  def conv(x: Seq[Int], h: Seq[Int]) = {
    val l = x.length + h.length - 1
    Range(0, l).map(n => Range(0, l).map(i => if (i >= x.length || n - i < 0 || n - i >= h.length) 0 else x(i) * h(n - i)).sum).toSeq
  }
}
