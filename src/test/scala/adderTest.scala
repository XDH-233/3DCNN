import org.scalatest.flatspec.AnyFlatSpec
import spinal.core._
import spinal.lib._

import spinal.core.sim._
import spinal.sim._

class adderTest extends AnyFlatSpec {

  SimConfig.withFstWave.compile(new adder(8)).doSim { dut =>
    import dut._
    Range(0, 1000).foreach { i =>
      io.a.randomize()
      io.b.randomize()
      sleep(1)
    }
  }
}
