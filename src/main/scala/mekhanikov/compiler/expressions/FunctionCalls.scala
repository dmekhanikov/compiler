package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser.FunctionCallContext
import mekhanikov.compiler.types.Primitives
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
    Option(LLVMGetNamedFunction(module, functionName)) match {
      case None =>
        throw new CompilationException(ctx, "call to an undeclared function")
      case Some(llvmFunction) =>
        val function = buildContext.functions(functionName)
        val args = Option(ctx.expressionList) match {
          case Some(argsExpr) =>
            argsExpr.expression.foldRight(List[Value]()) { (exprCtx, r) =>
              visitor.visit(exprCtx).get :: r
            }
          case None => List()
        }
        buildCall(function, args, ctx)
    }
  }

  def buildCall(function: Function, args: List[Value], ctx: ParserRuleContext): Value = {
    val providedArgTypes = args.map(arg => arg.valType)
    if (providedArgTypes != function.argTypes) {
      throw new CompilationException(ctx, "wrong function signature")
    }
    val llvmArgs = args.map(arg => arg.value)
    val callRes = LLVMBuildCall(builder, function.llvmFunction, new PointerPointer(llvmArgs: _*), args.size, "call")
    function.returnType match {
      case Primitives.VOID => Value.VOID
      case _ =>
        new Value(function.returnType, callRes)
    }
  }
}
