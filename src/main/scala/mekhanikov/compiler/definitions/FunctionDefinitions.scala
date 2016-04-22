package mekhanikov.compiler.definitions

import mekhanikov.compiler.ProgramParser.FunctionDefContext
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
    if (Option(LLVMGetNamedFunction(buildContext.module, llvmFunctionName)).isDefined) {
      throw new CompilationException(ctx, "function with this name already exists")
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
    val llvmFunction = buildContext.createFunction(llvmFunctionName, returnType, argTypes)
    val function = new Function(llvmFunctionName, returnType, argTypes, llvmFunction)
    buildContext.currentFunction = Some(llvmFunction)
    buildContext.functions(functionName) = function
    buildContext.variables.clear()
    Option(ctx.parameterList) match {
      case Some(parameterList) =>
        val di = if (currentStruct.isDefined) 1 else 0
        for (i <- 0 until parameterList.parameter.size + di) {
          val (varType: Type, varName: String) =
            if (i == 0 && currentStruct.isDefined) {
              (currentStruct.get, "this")
            } else {
              val parCtx = parameterList.parameter(i - di)
              val typeName = parCtx.ID(0).getText
              (buildContext.findType(typeName, parCtx), parCtx.ID(1).getText)
            }
          addVariable(varName, varType, llvmFunction, i)
        }
        for ((parCtx, i) <- parameterList.parameter.zipWithIndex) {
          val varType = buildContext.findType(parCtx.ID(0).getText, parCtx)
          val varName = parCtx.ID(1).getText
          val value = LLVMGetParam(llvmFunction, i)
          val parameter = new Variable(varType, varName)
          parameter.value = new Value(varType, value)
          buildContext.variables(varName) = parameter
        }
      case None =>
        if (currentStruct.isDefined) {
          addVariable("this", currentStruct.get, llvmFunction, 0)
        }
    }
    ctx.varDecl.foreach(visitor.visit)
    ctx.statement.foreach(visitor.visit)
    buildReturn(ctx, returnType)
    function
  }

  private def addVariable(varName: String, varType: Type, llvmFunction: LLVMValueRef, i: Int): Unit = {
    val value = LLVMGetParam(llvmFunction, 0)
    val parameter = new Variable(varType, varName)
    parameter.value = new Value(varType, value)
    buildContext.variables(varName) = parameter
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
