package mekhanikov.compiler.entities

import mekhanikov.compiler.Value
import mekhanikov.compiler.types.Type

class Variable(val varType: Type, val name: String) {
  var value: Option[Value] = None
}
