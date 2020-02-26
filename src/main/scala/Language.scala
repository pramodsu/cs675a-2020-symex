package symbolic

abstract class Expr
case class Var(name: String) extends Expr
case class Val(n: Int) extends Expr

// Arithmetic operators.
case class Plus(e1: Expr, e2: Expr) extends Expr
case class Neg(e: Expr) extends Expr

// Comparison operators.
case class Eq(e1: Expr, e2: Expr) extends Expr
case class Gt(e1: Expr, e2: Expr) extends Expr

// Boolean operators.
case class And(e1: Expr, e2: Expr) extends Expr
case class Not(e: Expr) extends Expr

// Statements.
abstract class Stmt
case class SkipStmt() extends Stmt
case class AssignStmt(lhs: String, rhs: Expr) extends Stmt
case class SeqStmt(s1: Stmt, s2: Stmt) extends Stmt
case class IfElseStmt(e: Expr, s1: Stmt, s2: Stmt) extends Stmt
case class AssertStmt(e: Expr) extends Stmt

object Stmt
{
  def fromList(stmt: List[Stmt]) : Stmt = {
    stmt match {
      case Nil => SkipStmt()
      case head :: rest => SeqStmt(head, fromList(rest))
    }
  }
}

object Interpreter {
  // Evaluate expressions.
  def eval(e: Expr, values : Map[String, Int]) : Int = {
    def _eval(e: Expr) = eval(e, values)
    def _toInt (v : Boolean) = if (v) 1 else 0
    def _toBool (v : Int) = v != 0
    e match {
      case Var(name) => values(name)
      case Val(n) => n
      // arith
      case Plus(e1, e2) => _eval(e1) + _eval(e2)
      case Neg(e) => -_eval(e)
      // cmp
      case Eq(e1, e2) => _toInt (_eval(e1) == _eval(e2))
      case Gt(e1, e2) => _toInt (_eval(e1) > _eval(e2))
      // bool
      case And(e1, e2) => _toInt(_toBool(_eval(e1)) && _toBool(_eval(e2)))
      case Not(e1) => _toInt(!_toBool(_eval(e)))
    }
  }
  // Execute statements
  def run(s: Stmt, values: Map[String, Int]) : Map[String, Int] = {
    s match {
      case SkipStmt() => values
      case AssignStmt(v, rhs) => values + (v -> eval(rhs, values))
      case SeqStmt(s1, s2) => run(s2, run(s1, values))
      case IfElseStmt(e, s1, s2) =>
        if (eval(e, values) != 0) {
          run(s1, values)
        } else {
          run(s2, values)
        }
      case AssertStmt(e) =>
        assert(eval(e, values) != 0)
        values
    }
  }

}
