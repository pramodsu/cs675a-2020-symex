import com.microsoft.z3

object Main
{
  def main(args: Array[String]): Unit = {
    val ctx = new z3.Context()
    val S = ctx.mkSolver()

    val a = ctx.mkIntConst("a")
    val b = ctx.mkIntConst("b")

    S.add(ctx.mkEq(ctx.mkAdd(a, b), ctx.mkInt(15)))
    println(S.check())
    println(S.getModel())
    
    puzzle(ctx, S)
  }

  //   SEND
  // + MORE
  // ------
  //  MONEY
  def puzzle(ctx : z3.Context, S : z3.Solver) : Unit = {
    val words = List("send", "more", "money")
    val letters = words.flatMap(w => w.map(_.toString()).toList).toSet
    println(letters)
    val vars = letters.map(l => (l -> ctx.mkIntConst(l))).toMap
  }

  /*
  def wordValue(word : String, vars : Map[String, z3.IntExpr]) : z3.IntExpr = {
  }
  */
}
