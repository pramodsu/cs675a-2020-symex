import com.microsoft.z3

object Main
{
  def main(args: Array[String]): Unit = {
    val ctx = new z3.Context()
    val S = ctx.mkSolver()

    val a = ctx.mkIntConst("a")
    val b = ctx.mkIntConst("b")

    //S.add(ctx.mkEq(ctx.mkAdd(a, b), ctx.mkInt(15)))
    //println(S.check())
    //println(S.getModel())
    
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
    println(vars)
    // d + n*10 + e*100 + s*1000
    val s_var = vars.get("s").get
    val w1_value = ctx.mkAdd(
                      vars("d"),
                      ctx.mkMul(vars("n"), ctx.mkInt(10)),
                      ctx.mkMul(vars("e"), ctx.mkInt(100)),
                      ctx.mkMul(vars("s"), ctx.mkInt(1000)))
    val w2_value = ctx.mkAdd(
                      vars("e"),
                      ctx.mkMul(vars("r"), ctx.mkInt(10)),
                      ctx.mkMul(vars("o"), ctx.mkInt(100)),
                      ctx.mkMul(vars("m"), ctx.mkInt(1000)))
    val w3_value = ctx.mkAdd(
                      vars("y"),
                      ctx.mkMul(vars("e"), ctx.mkInt(10)),
                      ctx.mkMul(vars("n"), ctx.mkInt(100)),
                      ctx.mkMul(vars("o"), ctx.mkInt(1000)),
                      ctx.mkMul(vars("m"), ctx.mkInt(10000)))
    println (w1_value)
    println (w2_value)
    println (w3_value)
    vars.foreach({
      case (k,v) => S.add(ctx.mkAnd(ctx.mkLe(ctx.mkInt(0), v), ctx.mkLe(v, ctx.mkInt(9))))
    })
    S.add(ctx.mkEq(ctx.mkAdd(w1_value, w2_value), w3_value))
    S.add(ctx.mkDistinct( vars("s"), vars("e"), vars("n"), 
            vars("d"), vars("m"), vars("o"), vars("r"), vars("y")))
    S.add(ctx.mkGt(vars("m"), ctx.mkInt(0)))
    println (S.check())
    val m = S.getModel()
    val w1 = m.eval(w1_value, true).asInstanceOf[z3.IntNum].getInt()
    val w2 = m.eval(w2_value, true)
    val w3 = m.eval(w3_value, true)
    println(w1)
    println(w2)
    println(w3)
  }

  /*
  def wordValue(word : String, vars : Map[String, z3.IntExpr]) : z3.IntExpr = {
  }
  */
}
