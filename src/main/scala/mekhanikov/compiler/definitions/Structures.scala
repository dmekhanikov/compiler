package mekhanikov.compiler.definitions

import mekhanikov.compiler.ProgramParser._
import mekhanikov.compiler.entities.Variable
import mekhanikov.compiler.entities.struct.{Field, Struct, Visibility}
import mekhanikov.compiler.types.Primitives
import mekhanikov.compiler.{BuildContext, CompilationException, Value}
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._

import scala.collection.JavaConversions._

class Structures(val buildContext: BuildContext) {

  val builder = buildContext.builder
  val visitor = buildContext.visitor

  def struct(ctx: StructDefContext): Unit = {
    val structName = ctx.ID.getText
    val fields = getFields(ctx.fieldDecl)
    val struct = new Struct(structName, fields)
    buildContext.structures(structName) = struct
  }

  def newExpr(ctx: NewExprContext): Value = {
    val structName = ctx.ID.getText
    if (!buildContext.structures.contains(structName)) {
      throw new CompilationException(ctx, "No such structure: " + structName)
    }
    val struct = buildContext.structures(structName)
    val llvmValue = LLVMBuildAlloca(builder, struct.toLLVMStructType, structName)
    struct.fields.zipWithIndex.foreach { case (field, i) =>
      val elementPtr = LLVMBuildStructGEP(builder, llvmValue, i, "fieldPtr")
      val initValue = if (field.fieldType.isInstanceOf[Struct]) {
        LLVMConstPointerNull(field.fieldType.toLLVMType)
      } else {
        LLVMConstNull(field.fieldType.toLLVMType)
      }
      LLVMBuildStore(builder, initValue, elementPtr)
    }
    new Value(struct, llvmValue)
  }

  def readAccess(ctx: FieldAccessContext): Value = {
    val varName = ctx.ID(0).getText
    val variable = findVariable(varName, ctx)
    val struct = variable.varType.asInstanceOf[Struct]
    val fieldName = ctx.ID(1).getText
    val (field, i) = findFieldWithIndex(struct, fieldName, ctx)
    val valuePtr = LLVMBuildStructGEP(builder, variable.value.value, i, "fieldPtr")
    val llvmValue = LLVMBuildLoad(builder, valuePtr, "field")
    new Value(field.fieldType, llvmValue)
  }

  def writeAccess(ctx: FieldAssignmentContext): Value = {
    val varName = ctx.ID(0).getText
    val variable = findVariable(varName, ctx)
    val struct = variable.varType.asInstanceOf[Struct]
    val fieldName = ctx.ID(1).getText
    val (field, i) = findFieldWithIndex(struct, fieldName, ctx)
    val value = visitor.visit(ctx.expression).get
    if (field.fieldType != value.valType) {
      throw new CompilationException(ctx, s"incompatible types: ${field.fieldType.name} and ${value.valType.name}")
    }
    val elementPtr = LLVMBuildStructGEP(builder, variable.value.value, i, "fieldPtr")
    LLVMBuildStore(builder, value.value, elementPtr)
    value
  }

  def findVariable(varName: String, ctx: ParserRuleContext): Variable = {
    buildContext.checkVariableExists(varName, ctx)
    val variable = buildContext.variables(varName)
    if (Primitives.isPrimitive(variable.varType)) {
      throw new CompilationException(ctx, "Cannot access a field of a primitive value")
    }
    variable
  }

  def findFieldWithIndex(struct: Struct, fieldName: String, ctx: ParserRuleContext): (Field, Int) = {
    struct.fields
      .zipWithIndex
      .find { case (field, i) => field.name == fieldName } match {
      case None =>
        throw new CompilationException(s"Structure ${struct.name} doesn't have a field $fieldName")
      case Some((field, i)) =>
        if (field.visibility == Visibility.PRIVATE
          && (buildContext.currentStructure.isEmpty ||
          buildContext.currentStructure.get != struct)) {
          throw new CompilationException(ctx, s"$fieldName is private in ${struct.name}")
        }
        (field, i)
    }
  }

  private def getFields(fieldsDecl: Iterable[FieldDeclContext]): List[Field] = {
    fieldsDecl.flatMap { fieldDeclCtx =>
      val visibility = if (Option(fieldDeclCtx.PRIVATE).isDefined) {
        Visibility.PRIVATE
      } else {
        Visibility.PUBLIC
      }
      val ids = fieldDeclCtx.varDecl.ID
      val fieldType = buildContext.findType(ids(0).getText, fieldDeclCtx)
      val fieldNames = ids.subList(1, ids.size)
      fieldNames.map { fieldNameCtx =>
        new Field(fieldNameCtx.getText, fieldType, visibility)
      }
    }.toList
  }
}
