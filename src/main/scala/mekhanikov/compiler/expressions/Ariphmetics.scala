package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser._
import mekhanikov.compiler.{BuildContext, CompilationException, Types, Value}
import org.bytedeco.javacpp.LLVM._

class Ariphmetics(val buildContext: BuildContext) {

  private val visitor = buildContext.visitor
  private val builder = buildContext.builder

  def sum(ctx: SumContext): Value = {
    val left = visitor.visit(ctx.expression(0)).get
    val right = visitor.visit(ctx.expression(1)).get
    if (left.typeName != Types.INT || right.typeName != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand types: (${left.typeName}, ${right.typeName})")
    }
    val result = ctx.SIGN.getSymbol.getText match {
      case "+" =>
        LLVMBuildAdd(builder, left.value, right.value, "add")
      case "-" =>
        LLVMBuildSub(builder, left.value, right.value, "sub")
    }
    new Value(Types.INT, result)
  }

  def muldiv(ctx: MulDivContext): Value = {
    val left = visitor.visit(ctx.expression(0)).get
    val right = visitor.visit(ctx.expression(1)).get
    if (left.typeName != Types.INT || right.typeName != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand types: (${left.typeName}, ${right.typeName})")
    }
    val result = ctx.MULDIV.getSymbol.getText match {
      case "*" =>
        LLVMBuildMul(builder, left.value, right.value, "mul")
      case "/" =>
        LLVMBuildSDiv(builder, left.value, right.value, "div")
      case "%" =>
        LLVMBuildSRem(builder, left.value, right.value, "mod")
    }
    new Value(Types.INT, result)
  }

  def sign(ctx: SignedExprContext): Value = {
    val value = visitor.visit(ctx.expression).get
    if (value.typeName != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand type: ${value.typeName}")
    }
    val resultValue = if (ctx.SIGN.getSymbol.getText == "-") {
      val zero = LLVMConstInt(LLVMInt32Type(), 0, 0)
      LLVMBuildSub(builder, zero, value.value, "sub")
    } else {
      value.value
    }
    new Value(value.typeName, resultValue)
  }

  def comparison(ctx: ComparisonContext): Value = {
    val operator: String = ctx.CMP().getSymbol.getText
    val left = visitor.visit(ctx.expression(0)).get
    val right = visitor.visit(ctx.expression(1)).get
    val typesConform = operator match {
      case "==" | "!=" =>
        (lt: String, rt: String) => lt == rt
      case _ =>
        (lt: String, rt: String) => lt == Types.INT && rt == Types.INT
    }
    if (!typesConform(left.typeName, right.typeName)) {
      throw new CompilationException(ctx, s"invalid operand types: (${left.typeName}, ${right.typeName})")
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
    new Value(Types.BOOLEAN, result)
  }

  def junction(ctx: JunctionContext): Value = {
    val left = visitor.visit(ctx.expression(0)).get
    val right = visitor.visit(ctx.expression(1)).get
    if (left.typeName != Types.BOOLEAN || right.typeName != Types.BOOLEAN) {
      throw new CompilationException(ctx, s"invalid operand types: (${left.typeName}, ${right.typeName})")
    }
    val operator = ctx.JUNCTION.getSymbol.getText
    val result = operator match {
      case "&&" => LLVMBuildAnd(builder, left.value, right.value, "and")
      case "||" => LLVMBuildOr(builder, left.value, right.value, "or")
    }
    new Value(Types.BOOLEAN, result)
  }
}
