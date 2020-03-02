package symbolic

import com.microsoft.z3
import scala.collection.mutable.Queue

object Symbolic
{
  val ctx = new z3.Context()
  
  var cnt = 0
  def newVar() : z3.IntExpr = {
    cnt += 1
    ctx.mkIntConst("input_" + cnt.toString())
  }

  type SymbolMap = Map[String, z3.IntExpr]

  def eval(e: Expr, values: SymbolMap) : z3.IntExpr = {
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
      // input
      case Input() => newVar()
    }
  }

  type State = (SymbolMap, z3.BoolExpr)
  def nextStates(st : Stmt, symt : SymbolMap, pc : z3.BoolExpr) : Stream[State] = {
    st match {
      case SkipStmt() => 
        val nextSt = (symt, pc)
        Stream(nextSt)
      case AssignStmt(lhs, rhs) =>
        val result = eval(rhs, symt)
        val nextSt = (symt + (lhs -> result), pc)
        Stream(nextSt)
      case SeqStmt(s1: Stmt, s2: Stmt) =>
        nextStates(s1, symt, pc).flatMap {
          case (symt2, pc2) => 
            nextStates(s2, symt2, pc2)
        }
      case IfElseStmt(e, s1, s2) =>
        val cond = ctx.mkDistinct(eval(e, symt), ctx.mkInt(0))
        nextStates(s1, symt, ctx.mkAnd(pc, cond)) #:::
          nextStates(s2, symt, ctx.mkAnd(pc, ctx.mkNot(cond)))
      case AssertStmt(e) =>
        val S = ctx.mkSolver()
        S.add(pc)
        S.add(ctx.mkEq(eval(e, symt), ctx.mkInt(0)))
        assert (S.check() == z3.Status.UNSATISFIABLE)
        val nextSt = (symt, pc)
        Stream(nextSt)
    }
  }
}
