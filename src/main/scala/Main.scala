package symbolic


object Main
{
  def main(args: Array[String]): Unit = {
    def noInput() : Int = 0

    val prog1 = AssignStmt("x", Val(1))
    println(Interpreter.run(prog1, Map.empty, noInput _))

    val prog2 = SeqStmt(
      AssignStmt("x", Val(1)),
      AssignStmt("y", Val(2))
    )
    println(Interpreter.run(prog2, Map.empty, noInput _))

    val prog3 = SeqStmt(
      AssignStmt("x", Val(1)),
      SeqStmt(
        AssignStmt("y", Val(2)),
        AssignStmt("z", Val(3))
      )
    )
    println(Interpreter.run(prog3, Map.empty, noInput _))

    val prog4 = Stmt.fromList(List(
      AssignStmt("x", Val(1)),
      AssignStmt("y", Val(2)),
      AssignStmt("z", Plus(Var("x"), Var("y"))),
      AssertStmt(Gt(Var("z"), Val(0)))
    ))
    println(Interpreter.run(prog4, Map.empty, noInput _))

    val prog5 = Stmt.fromList(List(
      AssignStmt("x", Input()),
      AssignStmt("y", Input()),
      AssignStmt("z", Plus(Var("x"), Var("y"))),
      AssertStmt(Gt(Var("z"), Val(0)))
    ))
    var inputList = List(10, 20)
    def inputFromList() : Int = {
      inputList match {
        case Nil => 0
        case head :: tail =>
          inputList = tail
          head
      }
    }
    println(Interpreter.run(prog5, Map.empty, inputFromList _))
    //println(Symbolic.nextStates(prog5, Map.empty, Symbolic.ctx.mkTrue()).toString())

    val prog6 = Stmt.fromList(List(
      AssignStmt("x", Input()),
      AssignStmt("y", Input()),
      AssignStmt("z", Plus(Var("x"), Var("y"))),
      IfElseStmt(And(Gt(Var("x"), Val(0)), 
                     Gt(Var("y"), Val(0))),
                 AssertStmt(Gt(Var("z"), Val(0))),
                 SkipStmt())
    ))
    println(Symbolic.nextStates(prog6, Map.empty, Symbolic.ctx.mkTrue()).toString())
  }
}
