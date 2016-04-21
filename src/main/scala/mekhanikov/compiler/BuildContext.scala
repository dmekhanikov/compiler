package mekhanikov.compiler

import mekhanikov.compiler.entities.{Function, Variable}
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.PointerPointer

import scala.collection.mutable

class BuildContext(val visitor: ProgramBaseVisitor[Option[Value]]) {

  val functions = mutable.HashMap[String, Function]() // name -> (return type, arguments types)
  var currentFunction: Option[LLVMValueRef] = None
  val variables = mutable.HashMap[String, Variable]()  // name -> (typename, value)

  val builder = LLVMCreateBuilder
  val module = LLVMModuleCreateWithName("module")

  lazy val numberFormat = LLVMBuildGlobalStringPtr(builder, "%d\n", "numberFormat")
  initIO()

  def checkVariableExists(varName: String, ctx: ParserRuleContext): Unit = {
    if (!variables.contains(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
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
    val printFun = createFunction(printFunName, Types.VOID, List(Types.INT))

    val value = LLVMGetParam(printFun, 0)
    val printfArgs = Array(numberFormat, value)
    LLVMBuildCall(builder, printf, new PointerPointer(printfArgs: _*), printfArgs.length, "print")
    LLVMBuildRetVoid(builder)
    functions(printFunName) = new Function(printFunName, Types.VOID, List(Types.INT), printFun)
  }

  private def generatePrintBool(): Unit = {
    val printf = LLVMGetNamedFunction(module, "printf")
    val printFunName = "printBool"
    val printFun = createFunction(printFunName, Types.VOID, List(Types.BOOLEAN))

    val value = LLVMGetParam(printFun, 0)
    val printfArgs = Array(numberFormat, value)
    LLVMBuildCall(builder, printf, new PointerPointer(printfArgs: _*), printfArgs.length, "print")
    LLVMBuildRetVoid(builder)
    functions(printFunName) = new Function(printFunName, Types.VOID, List(Types.BOOLEAN), printFun)
  }

  private def generateRead(): Unit = {
    val scanf = LLVMGetNamedFunction(module, "scanf")
    val readIntFunName = "readInt"
    val readIntFun = createFunction(readIntFunName, Types.INT, List())
    val tmp = LLVMBuildAlloca(builder, LLVMInt32Type(), "tmp")
    val scanfArgs = new PointerPointer[LLVMValueRef](numberFormat, tmp)
    LLVMBuildCall(builder, scanf, scanfArgs, 2, "scanf")
    val result = LLVMBuildLoad(builder, tmp, "read")
    LLVMBuildRet(builder, result)
    functions(readIntFunName) = new Function(readIntFunName, Types.INT, List(), readIntFun)
  }

  def createFunction(name: String, returnType: String, argTypes: Seq[String]): LLVMValueRef = {
    val returnTypeRef = Types.toTypeRef(returnType)
    val argTypeRefs = new PointerPointer(argTypes.map(typeName => Types.toTypeRef(typeName)):_*)
    val function = LLVMAddFunction(module, name, LLVMFunctionType(returnTypeRef, argTypeRefs, argTypes.size, 0))
    LLVMSetFunctionCallConv(function, LLVMCCallConv)
    val entry = LLVMAppendBasicBlock(function, "entry")
    LLVMPositionBuilderAtEnd(builder, entry)
    function
  }
}
