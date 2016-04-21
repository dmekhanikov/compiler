package mekhanikov.compiler.definitions

import mekhanikov.compiler.{BuildContext, CompilationException, Types}
import mekhanikov.compiler.ProgramParser.VarDeclContext
import mekhanikov.compiler.entities.Variable

import scala.collection.JavaConversions._

class VariableDeclarations(val buildContext: BuildContext) {

  def variable(ctx: VarDeclContext): Unit = {
    val typeName = ctx.ID(0).getSymbol.getText
    if (!List(Types.INT, Types.BOOLEAN).contains(typeName)) {
      throw new CompilationException(ctx, s"no such type: $typeName")
    }
    ctx.ID.subList(1, ctx.ID.size)
      .map { node => node.getSymbol.getText }
      .foreach { varName =>
        if (buildContext.variables.contains(varName)) {
          throw new CompilationException(ctx, s"repeated declaration of the variable $varName")
        }
        buildContext.variables(varName) = new Variable(typeName, varName)
      }
  }
}
