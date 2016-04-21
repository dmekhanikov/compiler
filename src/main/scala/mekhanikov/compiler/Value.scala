package mekhanikov.compiler

import mekhanikov.compiler.types.{Primitives, Type}
import org.bytedeco.javacpp.LLVM._

class Value(val valType: Type, val value: LLVMValueRef)

object Value {
  val VOID = new Value(Primitives.VOID, LLVMConstNull(LLVMVoidType()))
}
