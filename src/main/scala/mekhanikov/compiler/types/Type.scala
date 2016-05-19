package mekhanikov.compiler.types

import mekhanikov.compiler.entities.struct.Struct
import org.bytedeco.javacpp.LLVM.LLVMTypeRef

abstract class Type(val name: String) {
  def toLLVMType: LLVMTypeRef

  def isSubtypeOf(other: Type): Boolean = {
    this == other
  }
}

object Type {
  def lca(x: Type, y: Type): Type = {
    if (Primitives.isPrimitive(x) || Primitives.isPrimitive(y)) {
      if (x == y) {
        x
      } else {
        Primitives.VOID
      }
    } else {
      var xS = x.asInstanceOf[Struct]
      var yS = y.asInstanceOf[Struct]
      val xd = depth(xS)
      val yd = depth(yS)
      for (i <- 1 to xd - yd) { xS = xS.parentStruct.get }
      for (i <- 1 to yd - xd) { yS = yS.parentStruct.get }
      while (xS.parentStruct.isDefined && xS != yS) {
        xS = xS.parentStruct.get
        yS = yS.parentStruct.get
      }
      if (xS == yS) {
        xS
      } else {
        Primitives.VOID
      }
    }
  }

  private def depth(x: Struct): Int = {
    if (x.parentStruct.isDefined) {
      depth(x.parentStruct.get) + 1
    } else {
      1
    }
  }
}
