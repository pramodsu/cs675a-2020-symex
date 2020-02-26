package symbolic


object Main
{
  def main(args: Array[String]): Unit = {
    val prog1 = AssignStmt("x", Val(1))
    println(Interpreter.run(prog1, Map.empty))

    val prog2 = SeqStmt(
      AssignStmt("x", Val(1)),
      AssignStmt("y", Val(2))
    )
    println(Interpreter.run(prog2, Map.empty))

    val prog3 = SeqStmt(
      AssignStmt("x", Val(1)),
      SeqStmt(
        AssignStmt("y", Val(2)),
        AssignStmt("z", Val(3))
      )
    )
    println(Interpreter.run(prog3, Map.empty))

    val prog4 = Stmt.fromList(List(
      AssignStmt("x", Val(1)),
      AssignStmt("y", Val(2)),
      AssignStmt("z", Plus(Var("x"), Var("y"))),
      AssertStmt(Gt(Var("z"), Val(0)))
    ))
    println(Interpreter.run(prog4, Map.empty))

    val expr = Gt(Plus(Var("x"), Var("y")), Val(1))
    println(Symbolic.eval(expr, Map.empty))
  }
}
