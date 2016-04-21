package mekhanikov.compiler

import org.bytedeco.javacpp.LLVM._

class Value(val typeName: String, val value: LLVMValueRef)

object Value {
  val VOID = new Value(Types.VOID, null)
}
