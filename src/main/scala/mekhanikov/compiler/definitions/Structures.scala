package mekhanikov.compiler.definitions

import mekhanikov.compiler.ProgramParser._
import mekhanikov.compiler.entities.Function
import mekhanikov.compiler.entities.struct.Visibility.Visibility
import mekhanikov.compiler.entities.struct.{Field, Method, Struct, Visibility}
import mekhanikov.compiler.expressions.FunctionCalls
import mekhanikov.compiler.types.{Primitives, Type}
import mekhanikov.compiler.{BuildContext, CompilationException, Value}
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._

import scala.collection.JavaConversions._

class Structures(buildContext: BuildContext,
                 functionDefinitions: FunctionDefinitions,
                 functionCalls: FunctionCalls) {
  private val CONSTRUCTOR_METHOD_NAME = "~constructor"
  private val INIT_METHOD_NAME = "~init"
  private val builder = buildContext.builder
  private val visitor = buildContext.visitor

  def struct(ctx: StructDefContext): Unit = {
    val structName = ctx.ID.getText
    val parentStruct = findParentStruct(ctx)
    val struct = new Struct(structName, List(), List(), parentStruct)
    buildContext.structures(structName) = struct
    buildContext.currentStructure = Some(struct)
    ctx.memberDecl.foreach { memberDeclCtx =>
      val visibility = if (Option(memberDeclCtx.PRIVATE).isDefined) {
        Visibility.PRIVATE
      } else {
        Visibility.PUBLIC
      }
      if (Option(memberDeclCtx.fieldDecl).isDefined) {
        getFields(memberDeclCtx.fieldDecl, visibility).foreach { newField =>
          if (struct.allFields.exists(field => field.name == newField.name)) {
            throw new CompilationException(memberDeclCtx.fieldDecl, s"duplicated declaration of field ${newField.name}")
          } else {
            struct.fields ::= newField
          }
        }
      } else if (Option(memberDeclCtx.functionDef).isDefined) {
        val functionDefCtx = memberDeclCtx.functionDef
        val parameterListContext = Option(functionDefCtx.parameterList)
        val returnType = buildContext.findType(functionDefCtx.ID(0).getText, functionDefCtx)
        val functionName = functionDefCtx.ID(1).getText
        val function = functionDefinitions.functionHead(functionName, returnType, parameterListContext, functionDefCtx)
        val method = new Method(function, visibility)
        struct.methods ::= method
        functionDefinitions.functionBody(memberDeclCtx.functionDef.functionBody, function, returnType, parameterListContext)
      } else {
        val parameterListContext: Option[ParameterListContext] = Option(memberDeclCtx.constructorDef.parameterList)
        val function = functionDefinitions.functionHead(CONSTRUCTOR_METHOD_NAME, Primitives.VOID,
                                                        parameterListContext, memberDeclCtx.constructorDef.functionBody)
        val constructorFunction = functionDefinitions.functionBody(
          memberDeclCtx.constructorDef.functionBody,
          function,
          Primitives.VOID,
          parameterListContext)
        val method = new Method(constructorFunction, visibility)
        struct.methods ::= method
      }
    }
    val initMethod = createInitMethod(struct, ctx)
    if (!ctx.memberDecl.exists(memberDecl => Option(memberDecl.constructorDef).isDefined)) {
      val llvmInitFunction = initMethod.function.llvmFunction
      val constructorQualifiedName = s"${struct.name}/$CONSTRUCTOR_METHOD_NAME"
      val defaultConstructorFunction = new Function(constructorQualifiedName, Primitives.VOID, List(struct), llvmInitFunction)
      val constructor = new Method(defaultConstructorFunction, Visibility.PUBLIC)
      struct.methods ::= constructor
    }
    buildContext.currentStructure = None
  }

  private def findParentStruct(ctx: StructDefContext): Option[Struct] = {
    if (Option(ctx.parentStructDecl).isDefined) {
      val parentStructName = ctx.parentStructDecl.ID.getText
      buildContext.structures.get(parentStructName)
    } else {
      None
    }
  }

  private def createInitMethod(struct: Struct, ctx: StructDefContext): Method = {
    val function = functionDefinitions.functionHead(INIT_METHOD_NAME, Primitives.VOID, None, ctx)
    val thisRef = LLVMGetParam(function.llvmFunction, 0)
    struct.fields.zipWithIndex.foreach { case (field, i) =>
      val elementPtr = LLVMBuildStructGEP(builder, thisRef, i, "fieldPtr")
      val initValue = if (field.fieldType.isInstanceOf[Struct]) {
        LLVMConstPointerNull(field.fieldType.toLLVMType)
      } else {
        LLVMConstNull(field.fieldType.toLLVMType)
      }
      LLVMBuildStore(builder, initValue, elementPtr)
    }
    LLVMBuildRetVoid(builder)

    val method = new Method(function, Visibility.PUBLIC)
    struct.methods ::= method
    method
  }

  def newExpr(ctx: NewExprContext): Value = {
    val structName = ctx.ID.getText
    if (!buildContext.structures.contains(structName)) {
      throw new CompilationException(ctx, "No such structure: " + structName)
    }
    val struct = buildContext.structures(structName)
    val llvmValue = LLVMBuildMalloc(builder, struct.toLLVMStructType, structName)
    val thisValue = new Value(struct, llvmValue)
    buildCall(thisValue, INIT_METHOD_NAME, None, ctx)
    buildCall(thisValue, CONSTRUCTOR_METHOD_NAME, Option(ctx.expressionList), ctx)
    thisValue
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
    val methodName = ctx.ID.getText
    buildCall(expr, methodName, Option(ctx.expressionList), ctx)
  }

  private def buildCall(thisRef: Value,
                        methodName: String,
                        expressionListCtx: Option[ExpressionListContext],
                        callCtx: ParserRuleContext): Value = {
    val struct = thisRef.valType.asInstanceOf[Struct]
    var args = List(thisRef)
    expressionListCtx match {
      case Some(expressionList) =>
        args ++= expressionList.expression.map(exprCtx => visitor.visit(exprCtx).get)
      case None =>
    }
    val argTypes = args.map(arg => arg.valType)
    val method = findMethod(struct, methodName, argTypes, callCtx)
    functionCalls.buildCall(method.function, args, callCtx)
  }

  private def findMethod(struct: Struct, name: String, argTypes: Seq[Type], ctx: ParserRuleContext): Method = {
    val candidates = struct.allMethods.filter(
      method => method.name == name &&
                method.function.isApplicable(argTypes) &&
                isVisible(method.visibility, struct)
    )
    if (candidates.isEmpty) {
      throw new CompilationException(ctx, s"there is no method in struct ${struct.name} with such signature")
    } else if (candidates.size > 1) {
      throw new CompilationException(ctx, "ambigous call to overloaded method")
    } else {
      candidates.head
    }
  }

  private def findFieldWithIndex(value: Value, fieldName: String, ctx: ParserRuleContext): (Field, Int) = {
    if (Primitives.isPrimitive(value.valType)) {
      throw new CompilationException(ctx, "Cannot access a field of a primitive value")
    }
    val valueStruct = value.valType.asInstanceOf[Struct]
    valueStruct.allFields
      .zipWithIndex
      .find { case (field, i) => field.name == fieldName } match {
      case None =>
        throw new CompilationException(s"Structure ${valueStruct.name} doesn't have a field $fieldName")
      case Some((field, i)) =>
        if (!isVisible(field.visibility, valueStruct)) {
          throw new CompilationException(ctx, s"$fieldName is private in ${valueStruct.name}")
        }
        (field, i)
    }
  }

  private def isVisible(visibility: Visibility, struct: Struct): Boolean = {
    visibility == Visibility.PUBLIC ||
      buildContext.currentStructure.isDefined &&
      buildContext.currentStructure.get.isSubtypeOf(struct)
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
