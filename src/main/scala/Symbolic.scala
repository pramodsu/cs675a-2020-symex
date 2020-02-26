package symbolic

import com.microsoft.z3

object Symbolic
{
  val ctx = new z3.Context()

  def eval(e: Expr, values: Map[String, z3.IntExpr]) : z3.IntExpr = {
    def _eval(e: Expr) = eval(e, values)
    def _toInt (v : z3.BoolExpr) = 
      ctx.mkITE(v, ctx.mkInt(1), ctx.mkInt(0)).asInstanceOf[z3.IntExpr]
    def _toBool (v : z3.IntExpr) = 
      ctx.mkDistinct(v, ctx.mkInt(0))
    e match {
      case Var(name) =>
        values.get(name) match {
          case None => ctx.mkIntConst(name)
          case Some(v) => v
        }
      case Val(n) => ctx.mkInt(n)
      // arith
      case Plus(e1, e2) => ctx.mkAdd(_eval(e1), _eval(e2)).asInstanceOf[z3.IntExpr]
      case Neg(e) => ctx.mkUnaryMinus(_eval(e)).asInstanceOf[z3.IntExpr]
      // cmp
      case Eq(e1, e2) => _toInt (ctx.mkEq(_eval(e1), _eval(e2)))
      case Gt(e1, e2) => _toInt (ctx.mkGt(_eval(e1), _eval(e2)))
      // bool
      case And(e1, e2) => _toInt(ctx.mkAnd(_toBool(_eval(e1)), _toBool(_eval(e2))))
      case Not(e1) => _toInt(ctx.mkNot(_toBool(_eval(e))))
    }
  }

  def exec(st: Stmt) : Unit = {
    // TODO
  }
}
