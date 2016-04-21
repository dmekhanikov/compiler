package mekhanikov.compiler
import mekhanikov.compiler.ProgramParser.VarAssignmentContext

class AssignmentSearchVisitor(val buildContext: BuildContext) extends ProgramBaseVisitor[Set[String]] {

  override def aggregateResult(aggregate: Set[String], nextResult: Set[String]): Set[String] = {
    aggregate ++ nextResult
  }

  override def defaultResult(): Set[String] = {
    Set()
  }

  override def visitVarAssignment(ctx: VarAssignmentContext): Set[String] = {
    val varName = ctx.ID.getSymbol.getText
    buildContext.checkVariableExists(varName, ctx)
    Set(varName)
  }
}
