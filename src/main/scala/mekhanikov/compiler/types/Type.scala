package mekhanikov.compiler.types

import org.bytedeco.javacpp.LLVM.LLVMTypeRef

abstract class Type(val name: String) {
  def toLLVMType: LLVMTypeRef
}
