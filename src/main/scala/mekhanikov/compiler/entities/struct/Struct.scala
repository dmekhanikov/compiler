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
      parentStruct.get.allFields ++ fields
    } else {
      fields
    }
  }

  def allMethods: List[Method] = {
    var result = if (parentStruct.isDefined) {
      parentStruct.get.allMethods
    } else {
      List()
    }
    methods.foreach { method =>
      result.zipWithIndex.find { case (oldMethod, i) =>
          oldMethod.name == method.name &&
            oldMethod.function.argTypes.tail == method.function.argTypes.tail
      } match {
        case Some((oldMethod, i)) =>
          result = result.updated(i, method)
        case None =>
          result ++= List(method)
      }
    }
    result
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
