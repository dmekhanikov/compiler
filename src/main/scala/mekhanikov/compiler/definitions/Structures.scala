package mekhanikov.compiler.definitions

import mekhanikov.compiler.ProgramParser._
import mekhanikov.compiler.entities.struct.Visibility.Visibility
import mekhanikov.compiler.entities.struct.{Field, Method, Struct, Visibility}
import mekhanikov.compiler.expressions.FunctionCalls
import mekhanikov.compiler.types.Primitives
import mekhanikov.compiler.{BuildContext, CompilationException, Value}
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._

import scala.collection.JavaConversions._

class Structures(buildContext: BuildContext,
                 functionDefinitions: FunctionDefinitions,
                 functionCalls: FunctionCalls) {

  val builder = buildContext.builder
  val visitor = buildContext.visitor

  def struct(ctx: StructDefContext): Unit = {
    val structName = ctx.ID.getText
    val struct = new Struct(structName, List(), List())
    buildContext.structures(structName) = struct
    buildContext.currentStructure = Some(struct)
    ctx.memberDecl.foreach { memberDeclCtx =>
      val visibility = if (Option(memberDeclCtx.PRIVATE).isDefined) {
        Visibility.PRIVATE
      } else {
        Visibility.PUBLIC
      }
      Option(memberDeclCtx.fieldDecl) match {
        case Some(fieldDeclCtx) =>
          struct.fields ++= getFields(fieldDeclCtx, visibility)
        case None =>
          val methodDefContext = memberDeclCtx.functionDef()
          val function = functionDefinitions.function(methodDefContext)
          val method = new Method(function, visibility)
          struct.methods ::= method
      }
    }
    buildContext.currentStructure = None
  }

  def newExpr(ctx: NewExprContext): Value = {
    val structName = ctx.ID.getText
    if (!buildContext.structures.contains(structName)) {
      throw new CompilationException(ctx, "No such structure: " + structName)
    }
    val struct = buildContext.structures(structName)
    val llvmValue = LLVMBuildMalloc(builder, struct.toLLVMStructType, structName)
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

  def readAccess(ctx: FieldReadContext): Value = {
    val expr = visitor.visit(ctx.expression).get
    val fieldName = ctx.ID.getText
    val (field, i) = findFieldWithIndex(expr, fieldName, ctx)

    val valuePtr = LLVMBuildStructGEP(builder, expr.value, i, "fieldPtr")
    val llvmValue = LLVMBuildLoad(builder, valuePtr, "field")
    new Value(field.fieldType, llvmValue)
  }

  def writeAccess(ctx: FieldWriteContext): Value = {
    val expr = visitor.visit(ctx.expression(0)).get
    val fieldName = ctx.ID.getText
    val (field, i) = findFieldWithIndex(expr, fieldName, ctx)
    val value = visitor.visit(ctx.expression(1)).get
    if (field.fieldType != value.valType) {
      throw new CompilationException(ctx, s"incompatible types: ${field.fieldType.name} and ${value.valType.name}")
    }
    val elementPtr = LLVMBuildStructGEP(builder, expr.value, i, "fieldPtr")
    LLVMBuildStore(builder, value.value, elementPtr)
    value
  }

  def methodCall(ctx: MethodCallContext): Value = {
    val expr = visitor.visit(ctx.expression).get
    if (!expr.valType.isInstanceOf[Struct]) {
      throw new CompilationException(ctx, "Cannot invoke method of a primitive")
    }
    val struct = expr.valType.asInstanceOf[Struct]
    val methodName = ctx.ID.getText
    val optMethod = struct.methods.find(method => method.function.name == s"${struct.name}/$methodName")
    optMethod match {
      case Some(method) =>
        var args = List(expr)
        Option(ctx.expressionList) match {
          case Some(expressionListCtx) =>
            args ++= expressionListCtx.expression.map(exprCtx => visitor.visit(exprCtx).get)
          case None =>
        }
        val result = functionCalls.buildCall(method.function, args, ctx)
        result
      case None =>
        throw new CompilationException(ctx, s"Struct ${struct.name} doesn't have a method $methodName")
    }
  }

  private def findFieldWithIndex(value: Value, fieldName: String, ctx: ParserRuleContext): (Field, Int) = {
    if (Primitives.isPrimitive(value.valType)) {
      throw new CompilationException(ctx, "Cannot access a field of a primitive value")
    }
    val struct = value.valType.asInstanceOf[Struct]
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

  private def getFields(fieldsDeclCtx: FieldDeclContext, visibility: Visibility): List[Field] = {
    val ids = fieldsDeclCtx.ID
    val fieldType = buildContext.findType(ids(0).getText, fieldsDeclCtx)
    val fieldNames = ids.subList(1, ids.size)
    fieldNames.map { fieldNameCtx =>
      new Field(fieldNameCtx.getText, fieldType, visibility)
    }.toList
  }
}
