import scala.collection.mutable.ListBuffer
import scala.io.StdIn

class BrainfuckVm {
  val tape : ListBuffer[Int] = ListBuffer(0)
  var tapePosition : Int = 0

  def incrementPtr() = {
    if (tapePosition == tape.size-1)
      tape += 0
    tapePosition += 1
  }

  def decrementPtr() = {
    if (tapePosition == 0)
      throw new RuntimeException("Tape at start!")
    tapePosition -= 1
  }

  def increment() = {
    tape(tapePosition) += 1
  }

  def decrement() = {
    tape(tapePosition) -= 1
  }

  def output() = {
    print(tape(tapePosition).toChar)
  }

  def input() = {
    tape(tapePosition) = StdIn.readLine()(0)
  }

  def isZero: Boolean = {
    return tape(tapePosition) == 0
  }
}
