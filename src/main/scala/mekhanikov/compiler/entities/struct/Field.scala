package mekhanikov.compiler.entities.struct

import mekhanikov.compiler.entities.struct.Visibility.Visibility
import mekhanikov.compiler.types.Type

class Field(val name: String, val fieldType: Type, val visibility: Visibility)
