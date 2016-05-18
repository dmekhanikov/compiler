package mekhanikov.compiler.entities.struct

import mekhanikov.compiler.types.Type
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.PointerPointer

class Struct(name: String,
             var fields: List[Field],
             var methods: List[Method],
             val parentStruct: Option[Struct]) extends Type(name) {

  def allFields: List[Field] = {
    if (parentStruct.isDefined) {
      parentStruct.get.fields ++ fields
    } else {
      fields
    }
  }

  def toLLVMStructType: LLVMTypeRef = {
    val fieldTypes = allFields.map(field => field.fieldType.toLLVMType)
    LLVMStructType(new PointerPointer(fieldTypes:_*), fieldTypes.size, 0)
  }

  override def toLLVMType: LLVMTypeRef = {
    LLVMPointerType(toLLVMStructType, 0)
  }

  override def isSubtypeOf(other: Type): Boolean = {
    if (other == this) {
      true
    } else  other match {
      case struct: Struct if parentStruct.isDefined =>
        parentStruct.get.isSubtypeOf(other)
      case _ =>
        false
    }
  }
}
