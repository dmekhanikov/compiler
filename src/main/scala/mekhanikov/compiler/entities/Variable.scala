package mekhanikov.compiler.entities

import mekhanikov.compiler.Value

class Variable(val typeName: String, val name: String) {
  var value: Option[Value] = None
}
