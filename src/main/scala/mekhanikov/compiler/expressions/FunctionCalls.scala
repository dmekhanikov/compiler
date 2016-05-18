package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser.FunctionCallContext
import mekhanikov.compiler.types.{Primitives, Type}
import mekhanikov.compiler.{BuildContext, CompilationException, Value}
import mekhanikov.compiler.entities.Function
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.PointerPointer

import scala.collection.JavaConversions._

class FunctionCalls(val buildContext: BuildContext) {

  private val visitor = buildContext.visitor
  private val builder = buildContext.builder
  private val module = buildContext.module

  def call(ctx: FunctionCallContext): Value = {
    val functionName = ctx.ID.getSymbol.getText
    val args = Option(ctx.expressionList) match {
      case Some(argsExpr) =>
        argsExpr.expression.foldRight(List[Value]()) { (exprCtx, r) =>
          visitor.visit(exprCtx).get :: r
        }
      case None => List()
    }
    val argTypes = args.map { arg => arg.valType }
    val function = findFunction(buildContext.functions.values, functionName, argTypes, ctx)
    buildCall(function, args, ctx)
  }

  def buildCall(function: Function, args: List[Value], ctx: ParserRuleContext): Value = {
    val llvmArgs = args.zipWithIndex.map { case (arg, i) =>
      val expectedType = function.argTypes(i)
      if (arg.valType == expectedType) {
        arg.value
      } else {
        LLVMBuildBitCast(builder, arg.value, expectedType.toLLVMType, "cast")
      }
    }
    val resultPrefix =
      if (function.returnType != Primitives.VOID) "call" else ""
    val callRes = LLVMBuildCall(builder, function.llvmFunction, new PointerPointer(llvmArgs: _*), args.size, resultPrefix)
    function.returnType match {
      case Primitives.VOID => Value.VOID
      case _ =>
        new Value(function.returnType, callRes)
    }
  }

  private def findFunction(functions: Iterable[Function], name: String, argTypes: Seq[Type], ctx: ParserRuleContext): Function = {
    val candidates = functions.filter(function => function.name == name && isApplicable(function, argTypes))
    if (candidates.isEmpty) {
      throw new CompilationException(ctx, "there is no function with such signature")
    } else if (candidates.size > 1) {
      throw new CompilationException(ctx, "ambigous call to overloaded function")
    } else {
      candidates.head
    }
  }

  def isApplicable(function: Function, argTypes: Seq[Type]): Boolean = {
    if (function.argTypes.size != argTypes.size) {
      false
    } else {
      function.argTypes.zipWithIndex.forall { case (expectedType, i) => argTypes(i).isSubtypeOf(expectedType) }
    }
  }
}
