package mekhanikov.compiler.entities

import mekhanikov.compiler.types.Type
import org.bytedeco.javacpp.LLVM.LLVMValueRef

class Function(val name: String,
               val returnType: Type,
               val argTypes: List[Type],
               val llvmFunction: LLVMValueRef) {

  def isApplicable(argTypes: Seq[Type]): Boolean = {
    if (argTypes.size != this.argTypes.size) {
      false
    } else {
      this.argTypes.zipWithIndex.forall { case (expectedType, i) => argTypes(i).isSubtypeOf(expectedType) }
    }
  }
}
