package mekhanikov.compiler.entities

import org.bytedeco.javacpp.LLVM.LLVMValueRef

class Function(val name: String,
               val returnType: String,
               val argTypes: List[String],
               val llvmFunction: LLVMValueRef)
