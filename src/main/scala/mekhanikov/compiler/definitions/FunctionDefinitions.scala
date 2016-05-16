package mekhanikov.compiler.definitions

import mekhanikov.compiler.ProgramParser.{FunctionDefContext, ReturnStmtContext}
import mekhanikov.compiler._
import mekhanikov.compiler.entities.{Function, Variable}
import mekhanikov.compiler.types.{Primitives, Type}
import org.bytedeco.javacpp.LLVM._

import scala.collection.JavaConversions._

class FunctionDefinitions(val buildContext: BuildContext) {

  private val builder = buildContext.builder
  private val visitor = buildContext.visitor

  def function(ctx: FunctionDefContext): Function = {
    val returnType = buildContext.findType(ctx.ID(0).getText, ctx)
    val currentStruct = buildContext.currentStructure
    val functionName = ctx.ID(1).getText
    val llvmFunctionName = currentStruct match {
      case Some(struct) =>
        s"${struct.name}/$functionName"
      case None => functionName
    }
    var argTypes = Option(ctx.parameterList) match {
      case Some(parameterList) =>
        parameterList.parameter.map {parCtx =>
          buildContext.findType(parCtx.ID(0).getText, ctx)
        }.toList
      case None => List()
    }
    if (currentStruct.isDefined) {
        argTypes ::= currentStruct.get
    }
    val llvmFunction =
      try {
        buildContext.createFunction(llvmFunctionName, returnType, argTypes)
      } catch {
        case e: IllegalArgumentException =>
          throw new CompilationException(ctx, e.getMessage)
      }
    val function = new Function(llvmFunctionName, returnType, argTypes, llvmFunction)
    val signature = buildContext.functionSignature(function.name, function.argTypes)
    buildContext.currentFunction = Some(llvmFunction)
    buildContext.functions(signature) = function
    buildContext.variables.clear()
    val di = if (currentStruct.isDefined) {
        addVariable("this", currentStruct.get, llvmFunction, 0)
        1
      } else 0
    if (Option(ctx.parameterList).isDefined) {
      for ((parCtx, i) <- ctx.parameterList.parameter.zipWithIndex) {
        val typeName = parCtx.ID(0).getText
        val varType = buildContext.findType(typeName, parCtx)
        val varName = parCtx.ID(1).getText
        addVariable(varName, varType, llvmFunction, i + di)
      }
    }
    ctx.varDecl.foreach(visitor.visit)
    ctx.statement.foreach(visitor.visit)
    buildReturn(ctx, returnType)
    function
  }

  private def addVariable(varName: String, varType: Type, llvmFunction: LLVMValueRef, i: Int): Unit = {
    val value = LLVMGetParam(llvmFunction, i)
    val parameter = new Variable(varType, varName)
    parameter.value = new Value(varType, value)
    buildContext.variables(varName) = parameter
  }

  def buildReturn(ctx: FunctionDefContext, returnType: Type): Unit = {
    if (Option(ctx.returnStmt).isDefined && Option(ctx.returnStmt.expression).isDefined) {
      val returned = visitor.visit(ctx.returnStmt.expression).get
      if (returned.valType == returnType) {
        LLVMBuildRet(builder, returned.value)
      } else {
        throw new CompilationException(ctx, s"expecting expression of type ${returnType.name}, but was: ${returned.valType.name}")
      }
    } else if (returnType == Primitives.VOID) {
      LLVMBuildRetVoid(builder)
    } else {
      throw new CompilationException(ctx, "cannot return void from this function")
    }
  }
}
