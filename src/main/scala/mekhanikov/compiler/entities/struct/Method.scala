package mekhanikov.compiler.entities.struct

import mekhanikov.compiler.entities.struct.Visibility.Visibility
import mekhanikov.compiler.entities.Function

class Method(val function: Function, val visibility: Visibility) {
  val name = {
    val prefixLen = function.name.indexOf('/')
    if (prefixLen == -1) {
      function.name
    } else {
      function.name.substring(prefixLen + 1)
    }
  }
}
