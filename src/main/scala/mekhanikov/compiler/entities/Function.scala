package mekhanikov.compiler.entities

import mekhanikov.compiler.types.Type
import org.bytedeco.javacpp.LLVM.LLVMValueRef

class Function(val name: String,
               val returnType: Type,
               val argTypes: List[Type],
               val llvmFunction: LLVMValueRef)
