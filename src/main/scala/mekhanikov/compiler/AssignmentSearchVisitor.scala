package mekhanikov.compiler
import mekhanikov.compiler.ProgramParser.AssignmentExprContext

class AssignmentSearchVisitor(val buildContext: BuildContext) extends ProgramBaseVisitor[Set[String]] {

  override def aggregateResult(aggregate: Set[String], nextResult: Set[String]): Set[String] = {
    aggregate ++ nextResult
  }

  override def defaultResult(): Set[String] = {
    Set()
  }

  override def visitAssignmentExpr(ctx: AssignmentExprContext): Set[String] = {
    val varName = ctx.ID.getSymbol.getText
    buildContext.checkVariableExists(varName, ctx)
    Set(varName)
  }
}
