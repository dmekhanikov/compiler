package mekhanikov.compiler.definitions

import mekhanikov.compiler.{BuildContext, CompilationException}
import mekhanikov.compiler.ProgramParser.VarDeclContext
import mekhanikov.compiler.entities.Variable
import mekhanikov.compiler.types.Primitives

import scala.collection.JavaConversions._

class VariableDeclarations(val buildContext: BuildContext) {

  def variable(ctx: VarDeclContext): Unit = {
    val typeName = ctx.ID.getSymbol.getText
    val varType = buildContext.findType(typeName, ctx)
    if (varType == Primitives.VOID) {
      throw new CompilationException(ctx, "variable cannot have a void type")
    }
    ctx.varInit.foreach { varInitCtx =>
      val varName = varInitCtx.ID.getText
      if (buildContext.variables.contains(varName)) {
        throw new CompilationException(ctx, s"repeated declaration of the variable $varName")
      }
      val variable = new Variable(varType, varName)
      buildContext.variables(varName) = variable
      Option(varInitCtx.expression) match {
        case Some(exprContext) =>
          val value = buildContext.visitor.visit(exprContext).get
          if (value.valType != varType) {
            throw new CompilationException(varInitCtx, s"Incompatible types: ${varType.name} and ${value.valType}")
          }
          variable.value = value
        case None =>
      }
    }
  }
}
