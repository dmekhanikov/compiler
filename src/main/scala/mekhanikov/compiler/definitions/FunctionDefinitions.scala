package mekhanikov.compiler.definitions

import mekhanikov.compiler.ProgramParser.{FunctionBodyContext, FunctionDefContext, ParameterListContext}
import mekhanikov.compiler._
import mekhanikov.compiler.entities.struct.Struct
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
    functionBody(ctx.functionBody, functionName, returnType, Option(ctx.parameterList))
  }

  def functionBody(functionBodyCtx: FunctionBodyContext,
                   functionName: String,
                   returnType: Type,
                   parameterListCtx: Option[ParameterListContext]): Function = {
    val function = functionHead(functionName, returnType, parameterListCtx, functionBodyCtx)
    val functionSignature = buildContext.functionSignature(function.name, function.argTypes)
    buildContext.currentFunction = Some(function.llvmFunction)
    buildContext.functions(functionSignature) = function
    buildContext.variables.clear()
    val di = if (buildContext.currentStructure.isDefined) {
      addVariable("this", buildContext.currentStructure.get, function.llvmFunction, 0)
      1
    } else 0
    if (parameterListCtx.isDefined) {
      for ((parCtx, i) <- parameterListCtx.get.parameter.zipWithIndex) {
        val typeName = parCtx.ID(0).getText
        val varType = buildContext.findType(typeName, parCtx)
        val varName = parCtx.ID(1).getText
        addVariable(varName, varType, function.llvmFunction, i + di)
      }
    }
    functionBodyCtx.varDecl.foreach(visitor.visit)
    functionBodyCtx.statement.foreach(visitor.visit)
    buildReturn(functionBodyCtx, returnType)
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

  private def addVariable(varName: String, varType: Type, llvmFunction: LLVMValueRef, i: Int): Unit = {
    val value = LLVMGetParam(llvmFunction, i)
    val parameter = new Variable(varType, varName)
    parameter.value = new Value(varType, value)
    buildContext.variables(varName) = parameter
  }

  def buildReturn(ctx: FunctionBodyContext, returnType: Type): Unit = {
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
