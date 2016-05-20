package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser._
import mekhanikov.compiler.types.{LLVMAbortedValue, Primitives, Type}
import mekhanikov.compiler.{BuildContext, CompilationException, Value}
import mekhanikov.compiler.entities.Function
import org.antlr.v4.runtime.{ParserRuleContext, RuleContext}
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
    if (function == buildContext.currentFunction.get && !buildContext.tailCallReserved && isTailExpr(ctx)) {
      buildContext.tailCallReserved = true
      buildContext.sacc = Some(LLVMConstInt(LLVMInt32Type, 0, 0))
      buildContext.macc = Some(LLVMConstInt(LLVMInt32Type, 1, 0))
      val lastBlock = LLVMGetLastBasicBlock(function.llvmFunction)
      val di = if (buildContext.currentStructure.isDefined) 1 else 0
      buildContext.functionArguments.zipWithIndex.foreach { case (value, i) =>
        LLVMAddIncoming(value, args(i + di).value, lastBlock, 1)
      }
      new Value(function.returnType, LLVMAbortedValue)
    } else {
      val llvmArgs = args.zipWithIndex.map { case (arg, i) =>
        val expectedType = function.argTypes(i)
        buildContext.cast(arg, expectedType, ctx).value
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
  }

  private def findFunction(functions: Iterable[Function], name: String, argTypes: Seq[Type], ctx: ParserRuleContext): Function = {
    val candidates = functions.filter(function => function.name == name && function.isApplicable(argTypes))
    if (candidates.isEmpty) {
      throw new CompilationException(ctx, "there is no function with such signature")
    } else if (candidates.size > 1) {
      throw new CompilationException(ctx, "ambigous call to overloaded function")
    } else {
      candidates.head
    }
  }

  private def isTailExpr(ctx: RuleContext): Boolean = {
    ctx.parent match {
      case blockCtx: BlockContext => blockCtx.expression == ctx && isTailExpr(blockCtx)
      case condCtx: CondExprContext => (condCtx.block(0) == ctx || condCtx.block(1) == ctx) && isTailExpr(condCtx)
      case sumCtx: SumContext => isTailExpr(sumCtx)
      case mulDivCtx: MulDivContext if mulDivCtx.MULDIV.getText == "*" => isTailExpr(mulDivCtx)
      case signedExprCtx: SignedExprContext => isTailExpr(signedExprCtx)
      case parensExprCtx: ParensContext => isTailExpr(parensExprCtx)
      case functionBodyCtx: FunctionBodyContext => functionBodyCtx.expression == ctx
      case _ => false
    }
  }
}
