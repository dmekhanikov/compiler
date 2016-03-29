package mekhanikov.compiler

import org.bytedeco.javacpp.LLVM._

object Types {
  val VOID    = "void"
  val INT     = "int"
  val BOOLEAN = "bool"

  def toTypeRef(typeName: String): LLVMTypeRef = {
    typeName match {
      case INT      => LLVMInt32Type()
      case BOOLEAN  => LLVMInt1Type()
      case VOID     => LLVMVoidType()
    }
  }
}
