package mekhanikov.compiler
import mekhanikov.compiler.ProgramParser.AssignmentExprContext

class AssignmentSearchVisitor extends ProgramBaseVisitor[Set[String]] {
  override def aggregateResult(aggregate: Set[String], nextResult: Set[String]): Set[String] = {
    aggregate ++ nextResult
  }

  override def defaultResult(): Set[String] = {
    Set()
  }

  override def visitAssignmentExpr(ctx: AssignmentExprContext): Set[String] = {
    Set(ctx.ID.getSymbol.getText)
  }
}
