package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser.{VarAssignmentContext, VariableContext}
import mekhanikov.compiler.{BuildContext, CompilationException, Value}

class Variables(val buildContext: BuildContext) {

  def variable(ctx: VariableContext): Value = {
    val varName = ctx.ID.getSymbol.getText
    buildContext.checkVariableExists(varName, ctx)
    val variable = buildContext.variables(varName)
    variable.value
  }

  def assignment(ctx: VarAssignmentContext): Value = {
    val varName = ctx.ID.getSymbol.getText
    buildContext.checkVariableExists(varName, ctx)
    val variable = buildContext.variables(varName)
    val exprValue = buildContext.visitor.visit(ctx.expression).get
    variable.value = buildContext.cast(exprValue, variable.varType, ctx)
    variable.value
  }
}
