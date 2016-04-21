package mekhanikov.compiler.definitions

import mekhanikov.compiler.ProgramParser.FunctionDefContext
import mekhanikov.compiler.entities.{Function, Variable}
import mekhanikov.compiler._
import mekhanikov.compiler.types.{Primitives, Type}
import org.bytedeco.javacpp.LLVM._

import scala.collection.JavaConversions._

class FunctionDefinitions(val buildContext: BuildContext) {

  private val builder = buildContext.builder
  private val visitor = buildContext.visitor

  def function(ctx: FunctionDefContext): Unit = {
    val returnType = buildContext.findType(ctx.ID(0).getText, ctx)
    val functionName = ctx.ID(1).getText
    if (LLVMGetNamedFunction(buildContext.module, functionName) != null) {
      throw new CompilationException(ctx, "function with this name already exists")
    }
    val argTypes = Option(ctx.parameterList) match {
      case Some(parameterList) =>
        parameterList.parameter.map {parCtx =>
          buildContext.findType(parCtx.ID(0).getText, ctx)
        }.toList
      case None => List()
    }
    val llvmFunction = buildContext.createFunction(functionName, returnType, argTypes)
    buildContext.currentFunction = Some(llvmFunction)
    buildContext.functions(functionName) = new Function(functionName, returnType, argTypes, llvmFunction)
    buildContext.variables.clear()
    Option(ctx.parameterList) match {
      case Some(parameterList) =>
        for ((parCtx, i) <- parameterList.parameter.zipWithIndex) {
          val varType = buildContext.findType(parCtx.ID(0).getText, parCtx)
          val varName = parCtx.ID(1).getText
          val value = LLVMGetParam(llvmFunction, i)
          val parameter = new Variable(varType, varName)
          parameter.value = new Value(varType, value)
          buildContext.variables(varName) = parameter
        }
      case None =>
    }
    ctx.varDecl.foreach(visitor.visit)
    ctx.statement.foreach(visitor.visit)
    buildReturn(ctx, returnType)
  }

  private def buildReturn(ctx: FunctionDefContext, returnType: Type): Unit = {
    Option(ctx.expression) match {
      case Some(returnExpr) =>
        val returned = visitor.visit(returnExpr).get
        if (returned.valType == returnType) {
          LLVMBuildRet(builder, returned.value)
        } else {
          throw new CompilationException(ctx, s"expecting expression of type $returnType, but was: ${returned.valType.name}")
        }
      case None =>
        if (returnType == Primitives.VOID) {
          LLVMBuildRetVoid(builder)
        } else {
          throw new CompilationException(ctx, "cannot return void from this function")
        }
    }
  }
}
