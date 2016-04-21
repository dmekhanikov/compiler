package mekhanikov.compiler.entities.struct

import mekhanikov.compiler.types.Type
import org.bytedeco.javacpp.{LLVM, PointerPointer}
import org.bytedeco.javacpp.LLVM.LLVMTypeRef

class Struct(name: String, val fields: List[Field]) extends Type(name) {

  override def toLLVMType: LLVMTypeRef = {
    val fieldTypes = fields.map(field => field.fieldType.toLLVMType)
    LLVM.LLVMStructType(new PointerPointer(fieldTypes:_*), fieldTypes.size, 0)
  }
}
