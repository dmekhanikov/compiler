package mekhanikov.compiler

import mekhanikov.compiler.ProgramParser._
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.{BytePointer, Pointer, PointerPointer}

import scala.collection.JavaConversions._
import scala.collection.mutable

class CodegenProgramListener extends ProgramBaseListener {

  private val MAIN_METHOD_NAME = "main"
  private val MODULE_NAME = "module"

  private var module: LLVMModuleRef = null
  private var main: LLVMValueRef = null // only one function is supported for now
  private var entry: LLVMBasicBlockRef = null
  private var builder: LLVMBuilderRef = null
  private var expressionValues: List[(String, LLVMValueRef)] = null  // (typeName, valueRef)
  private var variables: mutable.HashMap[String, (String, LLVMValueRef)] = null  // name -> (typename, value)

  private var numberFormat: LLVMValueRef = null

  private var genIndex = 0

  private def generateName(prefix: String): String = {
    genIndex += 1
    prefix + genIndex
  }

  private def declarePrintf(): Unit = {
    val i8Pointer = LLVMPointerType(LLVMInt8Type(), 0)
    val fnType = LLVMFunctionType(LLVMInt32Type(), i8Pointer, 1, 1)
    val fn = LLVMAddFunction(module, "printf", fnType)
    LLVMSetFunctionCallConv(fn, LLVMCCallConv)
  }

  private def initIO(): Unit = {
    declarePrintf()
    numberFormat = LLVMBuildGlobalStringPtr(builder, "%d\n", "numberFormat")
  }

  override def enterProgram(ctx: ProgramContext): Unit = {
    module = LLVMModuleCreateWithName(MODULE_NAME)
    main = initMain()
    entry = LLVMAppendBasicBlock(main, "entry")
    builder = LLVMCreateBuilder
    LLVMPositionBuilderAtEnd(builder, entry)
    initIO()
    expressionValues = List()
    variables = mutable.HashMap()
  }

  override def exitProgram(ctx: ProgramContext): Unit = {
    LLVMBuildRetVoid(builder)
    LLVMDumpModule(module)
    val error = new BytePointer(null: Pointer)
    LLVMVerifyModule(module, LLVMAbortProcessAction, error)
    LLVMDisposeMessage(error)
  }

  private def initMain(): LLVMValueRef = {
    val main = LLVMAddFunction(module, MAIN_METHOD_NAME, LLVMFunctionType(LLVMVoidType, LLVMVoidType, 0, 0))
    LLVMSetFunctionCallConv(main, LLVMCCallConv)
    main
  }

  override def enterBoolConst(ctx: BoolConstContext): Unit = {
    val value = ctx.B.getSymbol.getText.toBoolean
    val valueRef = LLVMConstInt(LLVMInt1Type(), if (value) 1 else 0, 0)
    expressionValues = (Types.BOOLEAN, valueRef) :: expressionValues
  }

  override def enterIntConst(ctx: IntConstContext): Unit = {
    val value = ctx.Z.getSymbol.getText.toInt
    val valueRef = LLVMConstInt(LLVMInt32Type(), value, 0)
    expressionValues = (Types.INT, valueRef) :: expressionValues
  }

  override def enterVarDecl(ctx: VarDeclContext): Unit = {
    val typeName = ctx.ID(0).getSymbol.getText
    ctx.ID.subList(1, ctx.ID.size)
      .map { node => node.getSymbol.getText }
      .foreach { varName =>
        variables += (varName -> (typeName, null))
      }
  }

  override def enterVariable(ctx: VariableContext): Unit = {
    val varName = ctx.ID.getSymbol.getText
    checkVariableExists(varName, ctx)
    if (!variables.containsKey(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
    }
    val (typeName, value) = variables(varName)
    if (value == null) {
      throw new CompilationException(ctx, s"variable $varName is not initialized")
    }
    expressionValues = (typeName, value) :: expressionValues
  }

  override def exitAssignmentExpr(ctx: AssignmentExprContext): Unit = {
    val varName = ctx.ID.getSymbol.getText
    checkVariableExists(varName, ctx)
    val value = expressionValues.head
    variables.put(varName, value)
  }

  def checkVariableExists(varName: String, ctx: ParserRuleContext): Unit = {
    if (!variables.containsKey(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
    }
  }

  override def exitFunctionCall(ctx: FunctionCallContext): Unit = {
    val functionName = ctx.ID.getSymbol.getText
    val argumentsCount = ctx.expressionList.expression.size
    val arguments = expressionValues.take(argumentsCount)
    expressionValues = expressionValues.drop(argumentsCount)
    functionName match {
      case "print" =>
        if (argumentsCount != 1) {
          throw new CompilationException(ctx, "wrong number of arguments")
        }
        val (_, value) = arguments.head
        val printfArgs = Array(numberFormat, value)
        val printf = LLVMGetNamedFunction(module, "printf")
        LLVMBuildCall(builder, printf, new PointerPointer(printfArgs:_*), printfArgs.length, generateName("print"))
    }
  }

  override def exitExprStmt(ctx: ExprStmtContext): Unit = {
    expressionValues = List()
  }

  override def exitSum(ctx: SumContext): Unit = {
    val (left, right) = getOperands(Types.INT, ctx)
    val result = ctx.SIGN.getSymbol.getText match {
      case "+" =>
        LLVMBuildAdd(builder, left, right, generateName("add"))
      case "-" =>
        LLVMBuildSub(builder, left, right, generateName("sub"))
    }
    expressionValues = (Types.INT, result) :: expressionValues
  }

  override def exitMulDiv(ctx: MulDivContext): Unit = {
    val (left, right) = getOperands(Types.INT, ctx)
    val result = ctx.MULDIV.getSymbol.getText match {
      case "*" =>
        LLVMBuildMul(builder, left, right, generateName("mul"))
      case "/" =>
        LLVMBuildSDiv(builder, left, right, generateName("div"))
      case "%" =>
        LLVMBuildSRem(builder, left, right, generateName("mod"))
    }
    expressionValues = (Types.INT, result) :: expressionValues
  }

  override def exitSignedExpr(ctx: SignedExprContext): Unit = {
    if (ctx.SIGN.getSymbol.getText == "-") {
      val (typeName, value) = expressionValues.head
      if (typeName != Types.INT) {
        throw new CompilationException(ctx, s"invalid operand type: $typeName")
      }
      val zero = LLVMConstInt(LLVMInt32Type(), 0, 0)
      val resultValue = LLVMBuildSub(builder, zero, value, generateName("sub"))
      expressionValues = (typeName, resultValue) :: expressionValues.drop(1)
    }
  }

  private def getOperands(expectedType: String, ctx: ParserRuleContext): (LLVMValueRef, LLVMValueRef) = {
    val (rightType, right) = expressionValues.head
    val (leftType, left) = expressionValues(1)
    if (leftType != expectedType || rightType != expectedType) { // no autocasting
      throw new CompilationException(ctx, s"invalid operand types: ($leftType, $rightType)")
    }
    expressionValues = expressionValues.drop(2)
    (left, right)
  }
}
