package de.codecentric


object Main extends App {
  println("Hello World")

  {
    import initial.tagless.Interpreter._
    println(prettyPrint(sampleProgram))
  }

  {
    import `final`.ExprSym._
    println(sampleProgram[PP])
  }
}
