import chisel3._
import chisel3.util._

class Accelerator extends Module {
  val io = IO(new Bundle {
    val start = Input(Bool())
    val done = Output(Bool())

    val address = Output(UInt (16.W))
    val dataRead = Input(UInt (32.W))
    val writeEnable = Output(Bool ())
    val dataWrite = Output(UInt (32.W))
  
  })

  val idle :: read :: writeBlack :: erodePendingAbove :: erodePendingLeft :: erode :: erodeAndWritePendingAbove :: writePendingAbove :: done :: Nil = Enum (9)
  val stateReg = RegInit(idle)

  val x = RegInit(0.U(32.W))
  val y = RegInit(0.U(32.W))
  val onBorder = x === 0.U || x === 19.U || y === 0.U || y === 19.U
  val registers = RegInit(VecInit(Seq.fill(42)(false.B)))
  val nextDone = RegInit(false.B)

  io.address := y * 20.U + x
  io.writeEnable := false.B
  io.dataWrite := 0.U
  io.done := false.B


  switch(stateReg) {
    is(idle) {
      when(io.start) {
        stateReg := read
        x := 0.U
        y := 0.U
      }
    }

    is(read) {
      when(nextDone) {
        stateReg := done
        io.done := true.B
      }.otherwise {
        when(io.dataRead === 0.U) {
          stateReg := writeBlack
        }.otherwise {
          when (registers(x) && registers(x + 1.U) && !registers(x + 22.U) && !onBorder) {
            stateReg := read
            registers(x + 1.U) := true.B
            registers(x + 22.U) := true.B
            when(x === 19.U) {
              when (y === 19.U) {
                nextDone := true.B
              }.otherwise {
                x := 0.U
                y := y + 1.U
              }
            }.otherwise {
              x := x + 1.U
            }
          }
          when (registers(x + 22.U) && (onBorder || !registers(x))) {
            stateReg := erodeAndWritePendingAbove
            registers(x + 1.U) := true.B
            registers(x + 22.U) := false.B
          }

          when (!registers(x + 22.U) && (!registers(x) || !registers(x + 1.U) || onBorder)) {
            stateReg := erode
            registers(x + 1.U) := true.B
            registers(x + 22.U) := false.B
          }

          when (registers(x) && registers(x + 1.U) && registers(x + 22.U) && !onBorder) {
            stateReg := writePendingAbove
            registers(x + 1.U) := true.B
            registers(x + 22.U) := true.B
          }
        }
      }
    }

    is(writeBlack) {
      io.address := y * 20.U + x + 400.U
      io.writeEnable := true.B
      registers(x + 1.U) := false.B

      when(registers(x + 22.U)) {
        stateReg := erodePendingAbove
      }.otherwise {
        when(registers(x + 21.U)) {
          stateReg := erodePendingLeft
        }.otherwise {
          stateReg := read
          when(x === 19.U) {
            when (y === 19.U) {
              nextDone := true.B 
            }.otherwise {
              x := 0.U
              y := y + 1.U 
            }
          }.otherwise {
            x := x + 1.U 
          }
        }
      }
    }

    is(erodePendingAbove) {
      io.address := (y - 1.U) * 20.U + x + 400.U
      io.writeEnable := true.B
      registers(x + 22.U) := false.B
      when (registers(x + 21.U)) {
        stateReg := erodePendingLeft
      }.otherwise {
        stateReg := read
        when(x === 19.U) {
          when (y === 19.U) {
            nextDone := true.B 
          }.otherwise {
            x := 0.U
            y := y + 1.U 
          }
        }.otherwise {
          x := x + 1.U 
        }
      }
    }

    is(erodePendingLeft) {
      stateReg := read
      io.address := y * 20.U + x + 399.U
      io.writeEnable := true.B
      registers(x + 21.U) := false.B
      when(x === 19.U) {
        when (y === 19.U) {
          nextDone := true.B 
        }.otherwise {
          x := 0.U
          y := y + 1.U 
        }
      }.otherwise {
        x := x + 1.U 
      }
    }

    is(erodeAndWritePendingAbove) {
      stateReg := writePendingAbove
      io.address := y * 20.U + x + 400.U
      io.writeEnable := true.B
    }

    is (writePendingAbove) {
      stateReg := read
      io.address := (y - 1.U) * 20.U + x + 400.U
      io.writeEnable := true.B
      io.dataWrite := 255.U
      when(x === 19.U) {
        when (y === 19.U) {
          nextDone := true.B
        }.otherwise {
          x := 0.U
          y := y + 1.U
        }
      }.otherwise {
        x := x + 1.U
      }
    }

    is (erode) {
      stateReg := read
      io.address := y * 20.U + x + 400.U
      io.writeEnable := true.B
      when(x === 19.U) {
        when (y === 19.U) {
          nextDone := true.B
        }.otherwise {
          x := 0.U
          y := y + 1.U
        }
      }.otherwise {
        x := x + 1.U
      }
    }

    is (done) {
      stateReg := done
      io.done := true.B
    }
  }



  //Write here your code

}
