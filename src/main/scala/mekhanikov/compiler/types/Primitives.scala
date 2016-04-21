package mekhanikov.compiler.types

import org.bytedeco.javacpp.LLVM._

object Primitives {
  val INT = new Type("int") {
    override def toLLVMType: LLVMTypeRef = LLVMInt32Type()
  }

  val BOOLEAN = new Type("bool") {
    override def toLLVMType: LLVMTypeRef = LLVMInt1Type()
  }

  val VOID = new Type("void") {
    override def toLLVMType: LLVMTypeRef = LLVMVoidType()
  }

  def forName(name: String): Option[Type] = {
    name match {
      case INT.name => Some(INT)
      case BOOLEAN.name => Some(BOOLEAN)
      case VOID.name => Some(VOID)
      case _ => None
    }
  }

  def isPrimitive(typeObj: Type): Boolean = {
    typeObj == INT ||
    typeObj == BOOLEAN ||
    typeObj == VOID
  }
}
