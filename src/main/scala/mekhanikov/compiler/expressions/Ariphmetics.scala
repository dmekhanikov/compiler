package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser._
import mekhanikov.compiler._
import mekhanikov.compiler.types.{LLVMAbortedValue, Primitives, Type}
import org.bytedeco.javacpp.LLVM._

class Ariphmetics(val buildContext: BuildContext) {

  private val visitor = buildContext.visitor
  private val builder = buildContext.builder

  def sum(ctx: SumContext): Value = {
    val left = visitor.visit(ctx.expression(0)).get
    val right = visitor.visit(ctx.expression(1)).get
    if (left.valType != Primitives.INT || right.valType != Primitives.INT) {
      throw new CompilationException(ctx, s"invalid operand types: (${left.valType.name}, ${right.valType.name})")
    }
    if (left.value == LLVMAbortedValue || right.value == LLVMAbortedValue) {
      val (leftVal, rightVal) =
        if (left.value == LLVMAbortedValue) {
          (buildContext.sacc.get, right.value)
        } else {
          (left.value, buildContext.sacc.get)
        }
      val sacc = ctx.SIGN.getText match {
        case "+" =>
          LLVMBuildAdd(builder, leftVal, rightVal, "add")
        case "-" =>
          LLVMBuildSub(builder, leftVal, rightVal, "sub")
      }
      buildContext.sacc = Some(sacc)
      new Value(Primitives.INT, LLVMAbortedValue)
    } else {
      val result =
        ctx.SIGN.getText match {
          case "+" =>
            LLVMBuildAdd(builder, left.value, right.value, "add")
          case "-" =>
            LLVMBuildSub(builder, left.value, right.value, "sub")
        }
      new Value(Primitives.INT, result)
    }
  }

  def muldiv(ctx: MulDivContext): Value = {
    val left = visitor.visit(ctx.expression(0)).get
    val right = visitor.visit(ctx.expression(1)).get
    if (left.valType != Primitives.INT || right.valType != Primitives.INT) {
      throw new CompilationException(ctx, s"invalid operand types: (${left.valType.name}, ${right.valType.name})")
    }
    if (left.value == LLVMAbortedValue || right.value == LLVMAbortedValue) {
      // it only can be a multiplication
      val realVal = if (left.value == LLVMAbortedValue) right.value else left.value
      val sacc = LLVMBuildMul(builder, buildContext.sacc.get, realVal, "mul")
      val macc = LLVMBuildMul(builder, buildContext.macc.get, realVal, "mul")
      buildContext.sacc = Some(sacc)
      buildContext.macc = Some(macc)
      new Value(Primitives.INT, LLVMAbortedValue)
    } else {
      val result = ctx.MULDIV.getSymbol.getText match {
        case "*" =>
          LLVMBuildMul(builder, left.value, right.value, "mul")
        case "/" =>
          LLVMBuildSDiv(builder, left.value, right.value, "div")
        case "%" =>
          LLVMBuildSRem(builder, left.value, right.value, "mod")
      }
      new Value(Primitives.INT, result)
    }
  }

  def sign(ctx: SignedExprContext): Value = {
    val value = visitor.visit(ctx.expression).get
    if (value.valType != Primitives.INT) {
      throw new CompilationException(ctx, s"invalid operand type: ${value.valType.name}")
    }
    val zero = LLVMConstInt(LLVMInt32Type(), 0, 0)
    if (value.value == LLVMAbortedValue) {
      if (ctx.SIGN.getSymbol.getText == "-") {
        val acc = LLVMBuildSub(builder, zero, buildContext.sacc.get, "sub")
        buildContext.sacc = Some(acc)
      }
      new Value(value.valType, LLVMAbortedValue)
    } else {
      val resultValue = if (ctx.SIGN.getSymbol.getText == "-") {
        LLVMBuildSub(builder, zero, value.value, "sub")
      } else {
        value.value
      }
      new Value(value.valType, resultValue)
    }
  }

  def comparison(ctx: ComparisonContext): Value = {
    val operator: String = ctx.CMP().getSymbol.getText
    val left = visitor.visit(ctx.expression(0)).get
    val right = visitor.visit(ctx.expression(1)).get
    val typesConform = operator match {
      case "==" | "!=" =>
        (lt: Type, rt: Type) => lt == rt
      case _ =>
        (lt: Type, rt: Type) => lt == Primitives.INT && rt == Primitives.INT
    }
    if (!typesConform(left.valType, right.valType)) {
      throw new CompilationException(ctx, s"invalid operand types: (${left.valType.name}, ${right.valType.name})")
    }
    val predicate = operator match {
      case "==" => LLVMIntEQ
      case "!=" => LLVMIntNE
      case "<"  => LLVMIntSLT
      case "<=" => LLVMIntSLE
      case ">"  => LLVMIntSGT
      case ">=" => LLVMIntSGE
    }
    val result = LLVMBuildICmp(builder, predicate, left.value, right.value, "cmp")
    new Value(Primitives.BOOLEAN, result)
  }

  def junction(ctx: JunctionContext): Value = {
    val left = visitor.visit(ctx.expression(0)).get
    val right = visitor.visit(ctx.expression(1)).get
    if (left.valType != Primitives.BOOLEAN || right.valType != Primitives.BOOLEAN) {
      throw new CompilationException(ctx, s"invalid operand types: (${left.valType.name}, ${right.valType.name})")
    }
    val operator = ctx.JUNCTION.getSymbol.getText
    val result = operator match {
      case "&&" => LLVMBuildAnd(builder, left.value, right.value, "and")
      case "||" => LLVMBuildOr(builder, left.value, right.value, "or")
    }
    new Value(Primitives.BOOLEAN, result)
  }
}
