package mekhanikov.compiler.expressions

import mekhanikov.compiler.ProgramParser.{BoolConstContext, IntConstContext}
import mekhanikov.compiler.types.Primitives
import mekhanikov.compiler.{BuildContext, Value}
import org.bytedeco.javacpp.LLVM._

class Constants(val buildContext: BuildContext) {

  def bool(ctx: BoolConstContext): Value = {
    val value = ctx.B.getSymbol.getText.toBoolean
    val valueRef = LLVMConstInt(LLVMInt1Type(), if (value) 1 else 0, 0)
    new Value(Primitives.BOOLEAN, valueRef)
  }

  def int(ctx: IntConstContext): Value = {
    val value = ctx.Z.getSymbol.getText.toInt
    val valueRef = LLVMConstInt(LLVMInt32Type(), value, 0)
    new Value(Primitives.INT, valueRef)
  }
}
