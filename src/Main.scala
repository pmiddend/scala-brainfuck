import javax.xml.stream.events.StartDocument

object Main {

  def jumpForward(s: String): Int = {
    var counter = 1
    var i = 0
    while (counter != 0) {
      if (s(i) == ']')
        counter -= 1
      else if (s(i) == '[')
        counter += 1
      i += 1
    }
    i
  }

  def jumpBackward(s: String, start: Int): Int = {
    var counter = 1
    var i = start
    while (counter != 0) {
      if (s(i) == ']')
        counter += 1
      else if (s(i) == '[')
        counter -= 1
      i -= 1
    }
    i + 2
  }

  def interpret(x: String): Unit = {
    val vm = new BrainfuckVm
    var inputIndex = 0
    while (inputIndex != x.size) x(inputIndex) match {
      case '>' => vm.incrementPtr(); inputIndex += 1
      case '<' => vm.decrementPtr(); inputIndex += 1
      case '+' => vm.increment(); inputIndex += 1
      case '-' => vm.decrement(); inputIndex += 1
      case '.' => vm.output(); inputIndex += 1
      case ',' => vm.input(); inputIndex += 1
      case '[' => if (vm.isZero) {
        inputIndex = jumpForward(x.substring(inputIndex + 1))
      } else inputIndex += 1
      case ']' => if (vm.isZero) {
        inputIndex += 1
      } else {
        inputIndex = jumpBackward(x, inputIndex - 1)
      }
      case _ => inputIndex += 1
    }
  }

  def main(args: Array[String]): Unit = {
    val inputString = ">+++++++++[<++++++++>-]<.>+++++++[<++++>-]<+.+++++++..+++.[-]>++++++++[<++++>-] <.>+++++++++++[<++++++++>-]<-.--------.+++.------.--------.[-]>++++++++[<++++>- ]<+.[-]++++++++++."
    println("Vorher: " + inputString)
    interpret(inputString)
  }
}
