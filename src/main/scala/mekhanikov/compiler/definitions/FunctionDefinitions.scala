package mekhanikov.compiler.definitions

import mekhanikov.compiler.ProgramParser._
import mekhanikov.compiler._
import mekhanikov.compiler.entities.{Function, Variable}
import mekhanikov.compiler.types.{Primitives, Type}
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._

import scala.collection.JavaConversions._

class FunctionDefinitions(val buildContext: BuildContext) {

  private val builder = buildContext.builder
  private val visitor = buildContext.visitor

  def function(ctx: FunctionDefContext): Function = {
    val returnType = buildContext.findType(ctx.ID(0).getText, ctx)
    val functionName = ctx.ID(1).getText
    val function = functionHead(functionName, returnType, Option(ctx.parameterList), ctx.functionBody)
    functionBody(ctx.functionBody, function, returnType, Option(ctx.parameterList))
  }

  def functionBody(functionBodyCtx: FunctionBodyContext,
                   function: Function,
                   returnType: Type,
                   parameterListCtx: Option[ParameterListContext]): Function = {
    val functionSignature = buildContext.functionSignature(function.name, function.argTypes)
    buildContext.currentFunction = Some(function)
    buildContext.functions(functionSignature) = function
    buildContext.variables.clear()
    val di = if (buildContext.currentStructure.isDefined) {
      val varValue = LLVMGetParam(function.llvmFunction, 0)
      addVariable("this", buildContext.currentStructure.get, function.llvmFunction, varValue)
      1
    } else 0
    val functionStart = LLVMAppendBasicBlock(function.llvmFunction, "functionStart")
    buildContext.functionStartBlock = Some(functionStart)
    val prevBlock = LLVMGetPreviousBasicBlock(functionStart)
    LLVMBuildBr(builder, functionStart)
    LLVMPositionBuilderAtEnd(builder, functionStart)
    if (parameterListCtx.isDefined) {
      for ((parCtx, i) <- parameterListCtx.get.parameter.zipWithIndex) {
        val typeName = parCtx.ID(0).getText
        val varType = buildContext.findType(typeName, parCtx)
        val varName = parCtx.ID(1).getText
        val varValue = LLVMGetParam(function.llvmFunction, i + di)
        val varPhi = LLVMBuildPhi(builder, varType.toLLVMType, "varPhi")
        LLVMAddIncoming(varPhi, varValue, prevBlock, 1)
        buildContext.functionArguments ++= List(varPhi)
        addVariable(varName, varType, function.llvmFunction, varPhi)
      }
    }
    functionBodyCtx.varDecl.foreach(visitor.visit)
    functionBodyCtx.statement.foreach(visitor.visit)
    buildReturn(functionBodyCtx, returnType)
    buildContext.functionArguments = List()
    function
  }

  def functionHead(functionName: String,
                   returnType: Type,
                   parameterListCtx: Option[ParameterListContext],
                   parentContext: ParserRuleContext): Function = {
    val qualifiedFunctionName = buildContext.currentStructure match {
      case Some(struct) =>
        s"${struct.name}/$functionName"
      case None => functionName
    }
    var argTypes = parameterListCtx match {
      case Some(parameterList) =>
        parameterList.parameter.map {parCtx =>
          buildContext.findType(parCtx.ID(0).getText, parentContext)
        }.toList
      case None => List()
    }
    if (buildContext.currentStructure.isDefined) {
      argTypes ::= buildContext.currentStructure.get
    }
    val functionSignature = buildContext.functionSignature(qualifiedFunctionName, argTypes)
    val llvmFunction =
      try {
        buildContext.createFunction(functionSignature, returnType, argTypes)
      } catch {
        case e: IllegalArgumentException =>
          throw new CompilationException(parentContext, e.getMessage)
      }
    new Function(qualifiedFunctionName, returnType, argTypes, llvmFunction)
  }

  private def addVariable(varName: String, varType: Type, llvmFunction: LLVMValueRef, value: LLVMValueRef): Unit = {
    val parameter = new Variable(varType, varName)
    parameter.value = new Value(varType, value)
    buildContext.variables(varName) = parameter
  }

  def buildReturn(ctx: FunctionBodyContext, returnType: Type): Unit = {
    if (returnType == Primitives.VOID) {
      if (Option(ctx.expression).isDefined) {
        visitor.visit(ctx.expression)
      }
      LLVMBuildRetVoid(builder)
    } else if (Option(ctx.expression).isDefined) {
      val exprValue = visitor.visit(ctx.expression).get
      if (exprValue.valType == Type.ABORTED) {
        throw new CompilationException(ctx, "this function may never terminate")
      }
      val returned = buildContext.cast(exprValue, returnType, ctx.expression)
      LLVMBuildRet(builder, returned.value)
    }  else {
      throw new CompilationException(ctx, "cannot return void from this function")
    }
  }
}
