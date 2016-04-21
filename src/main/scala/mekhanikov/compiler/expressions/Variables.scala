package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser.{AssignmentExprContext, VariableContext}
import mekhanikov.compiler.{BuildContext, CompilationException, Value}

class Variables(val buildContext: BuildContext) {

  def variable(ctx: VariableContext): Value = {
    val varName = ctx.ID.getSymbol.getText
    buildContext.checkVariableExists(varName, ctx)
    val variable = buildContext.variables(varName)
    variable.value match {
      case Some(value) => value
      case None =>
        throw new CompilationException(ctx, s"variable $varName is not initialized")
    }
  }

  def assignment(ctx: AssignmentExprContext): Value = {
    val varName = ctx.ID.getSymbol.getText
    buildContext.checkVariableExists(varName, ctx)
    val variable = buildContext.variables(varName)
    val exprValue = buildContext.visitor.visit(ctx.expression).get
    if (variable.typeName != exprValue.typeName) {
      throw new CompilationException(ctx, s"incompatible types: (${variable.typeName}, ${exprValue.typeName})")
    }
    variable.value = Some(exprValue)
    variable.value.get
  }
}
