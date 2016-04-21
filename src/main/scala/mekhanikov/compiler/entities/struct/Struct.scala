package mekhanikov.compiler.entities.struct

import mekhanikov.compiler.types.Type
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.PointerPointer

class Struct(name: String, val fields: List[Field]) extends Type(name) {

  def toLLVMStructType: LLVMTypeRef = {
    val fieldTypes = fields.map(field => field.fieldType.toLLVMType)
    LLVMStructType(new PointerPointer(fieldTypes:_*), fieldTypes.size, 0)
  }

  override def toLLVMType: LLVMTypeRef = {
    LLVMPointerType(toLLVMStructType, 0)
  }
}
