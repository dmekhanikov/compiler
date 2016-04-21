package mekhanikov.compiler.definitions

import mekhanikov.compiler.{BuildContext, CompilationException}
import mekhanikov.compiler.ProgramParser.VarDeclContext
import mekhanikov.compiler.entities.Variable
import mekhanikov.compiler.types.Primitives

import scala.collection.JavaConversions._

class VariableDeclarations(val buildContext: BuildContext) {

  def variable(ctx: VarDeclContext): Unit = {
    val typeName = ctx.ID(0).getSymbol.getText
    val varType = buildContext.findType(typeName, ctx)
    if (varType == Primitives.VOID) {
      throw new CompilationException(ctx, "variable cannot have a void type")
    }
    ctx.ID.subList(1, ctx.ID.size)
      .map { node => node.getSymbol.getText }
      .foreach { varName =>
        if (buildContext.variables.contains(varName)) {
          throw new CompilationException(ctx, s"repeated declaration of the variable $varName")
        }
        buildContext.variables(varName) = new Variable(varType, varName)
      }
  }
}
