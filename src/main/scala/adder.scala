import spinal.core._
import spinal.lib._

case class adder(w: Int) extends Component {
  val io = new Bundle {
    val a = in UInt (w bits)
    val b = in UInt (w bits)
    val c = out UInt (w bits)
  }

  io.c := io.a + io.b
}
