package mekhanikov.compiler

import mekhanikov.compiler.entities.struct.Struct
import mekhanikov.compiler.entities.{Function, Variable}
import mekhanikov.compiler.types.{Primitives, Type}
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.PointerPointer

import scala.collection.mutable

class BuildContext(val visitor: ProgramBaseVisitor[Option[Value]]) {

  val functions = mutable.HashMap[String, Function]() // name -> (return type, arguments types)
  val structures = mutable.HashMap[String, Struct]()
  var currentFunction: Option[Function] = None
  var currentStructure: Option[Struct] = None
  val variables = mutable.HashMap[String, Variable]()  // name -> (typename, value)

  // TCO-related
  var functionArguments = List[LLVMValueRef]()
  var functionStartBlock: Option[LLVMBasicBlockRef] = None
  var saccPhi: Option[LLVMValueRef] = None
  var sacc: Option[LLVMValueRef] = None
  var maccPhi: Option[LLVMValueRef] = None
  var macc: Option[LLVMValueRef] = None
  var tailCallReserved = false

  val builder = LLVMCreateBuilder
  val module = LLVMModuleCreateWithName("module")

  lazy val numberFormat = LLVMBuildGlobalStringPtr(builder, "%d\n", "numberFormat")
  initIO()

  def checkVariableExists(varName: String, ctx: ParserRuleContext): Unit = {
    if (!variables.contains(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
    }
  }

  def findType(typeName: String, ctx: ParserRuleContext): Type = {
    Primitives.forName(typeName) match {
      case Some(primType) => primType
      case None =>
        structures.get(typeName) match {
          case Some (struct) => struct
          case None =>
            throw new CompilationException(ctx, s"no such type: $typeName")
        }
    }
  }

  private def initIO(): Unit = {
    declareIOFun("printf")
    declareIOFun("scanf")
    generatePrintInt()
    generatePrintBool()
    generateRead()
  }

  private def declareIOFun(name: String): Unit = {
    val i8Pointer = LLVMPointerType(LLVMInt8Type(), 0)
    val funType = LLVMFunctionType(LLVMInt32Type(), i8Pointer, 1, 1)
    val fun = LLVMAddFunction(module, name, funType)
    LLVMSetFunctionCallConv(fun, LLVMCCallConv)
  }

  private def generatePrintInt(): Unit = {
    val printf = LLVMGetNamedFunction(module, "printf")
    val printFunName = "printInt"
    val printFun = createFunction(printFunName, Primitives.VOID, List(Primitives.INT))

    val value = LLVMGetParam(printFun, 0)
    val printfArgs = Array(numberFormat, value)
    LLVMBuildCall(builder, printf, new PointerPointer(printfArgs: _*), printfArgs.length, "print")
    LLVMBuildRetVoid(builder)
    functions(printFunName) = new Function(printFunName, Primitives.VOID, List(Primitives.INT), printFun)
  }

  private def generatePrintBool(): Unit = {
    val printf = LLVMGetNamedFunction(module, "printf")
    val printFunName = "printBool"
    val printFun = createFunction(printFunName, Primitives.VOID, List(Primitives.BOOLEAN))

    val value = LLVMGetParam(printFun, 0)
    val printfArgs = Array(numberFormat, value)
    LLVMBuildCall(builder, printf, new PointerPointer(printfArgs: _*), printfArgs.length, "print")
    LLVMBuildRetVoid(builder)
    functions(printFunName) = new Function(printFunName, Primitives.VOID, List(Primitives.BOOLEAN), printFun)
  }

  private def generateRead(): Unit = {
    val scanf = LLVMGetNamedFunction(module, "scanf")
    val readIntFunName = "readInt"
    val readIntFun = createFunction(readIntFunName, Primitives.INT, List())
    val tmp = LLVMBuildAlloca(builder, LLVMInt32Type(), "tmp")
    val scanfArgs = new PointerPointer[LLVMValueRef](numberFormat, tmp)
    LLVMBuildCall(builder, scanf, scanfArgs, 2, "scanf")
    val result = LLVMBuildLoad(builder, tmp, "read")
    LLVMBuildRet(builder, result)
    functions(readIntFunName) = new Function(readIntFunName, Primitives.INT, List(), readIntFun)
  }

  def functionSignature(name: String, argTypes: Seq[Type]): String = {
    val sb = new StringBuilder(name)
    for (argType <- argTypes) {
      sb.append(' ').append(argType.name)
    }
    sb.toString
  }

  def createFunction(signature: String, returnType: Type, argTypes: Seq[Type]): LLVMValueRef = {
    val argTypeRefs = new PointerPointer(argTypes.map(argType => argType.toLLVMType):_*)
    if (Option(LLVMGetNamedFunction(module, signature)).isDefined) {
      throw new IllegalArgumentException("function with this signature already exists: " + signature)
    }
    val function = LLVMAddFunction(module, signature, LLVMFunctionType(returnType.toLLVMType, argTypeRefs, argTypes.size, 0))
    LLVMSetFunctionCallConv(function, LLVMCCallConv)
    val entry = LLVMAppendBasicBlock(function, "entry")
    LLVMPositionBuilderAtEnd(builder, entry)
    function
  }

  def cast(value: Value, destType: Type, ctx: ParserRuleContext): Value = {
    if (value.valType != destType) {
      if (value.valType.isSubtypeOf(destType)) {
        val llvmValue = LLVMBuildBitCast(builder, value.value, destType.toLLVMType, "cast")
        new Value(destType, llvmValue)
      } else {
        throw new CompilationException(ctx, s"Incompatible types: ${destType.name} and ${value.valType.name}")
      }
    } else {
      value
    }
  }
}
