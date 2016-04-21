package mekhanikov.compiler.entities

import mekhanikov.compiler.Value
import mekhanikov.compiler.entities.struct.Struct
import mekhanikov.compiler.types.Type
import org.bytedeco.javacpp.LLVM._

class Variable(val varType: Type, val name: String) {
  var value: Value = {
    val initValue = if (varType.isInstanceOf[Struct]) {
      LLVMConstPointerNull(varType.toLLVMType)
    } else {
      LLVMConstNull(varType.toLLVMType)
    }
    new Value(varType, initValue)
  }
}
