package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser.FunctionCallContext
import mekhanikov.compiler.types.Primitives
import mekhanikov.compiler.{BuildContext, CompilationException, Value}
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
            if (argsExpr.expression.size != function.argTypes.size) {
              throw new CompilationException(ctx, "wrong number of arguments")
            }
            val providedArgs = argsExpr.expression.foldRight(List[Value]()) { (exprCtx, r) =>
              visitor.visit(exprCtx).get :: r
            }
            val providedArgTypes = providedArgs.map(arg => arg.valType)
            if (providedArgTypes != function.argTypes) {
              throw new CompilationException(ctx, "wrong function signature")
            }
            providedArgs.map(arg => arg.value)
          case None => List()
        }
        val callRes = LLVMBuildCall(builder, llvmFunction, new PointerPointer(args: _*), args.size, "call")
        function.returnType match {
          case Primitives.VOID => Value.VOID
          case _ =>
            new Value(function.returnType, callRes)
        }
    }
  }
}
