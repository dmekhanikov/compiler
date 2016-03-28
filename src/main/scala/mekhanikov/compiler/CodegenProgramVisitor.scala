package mekhanikov.compiler

import mekhanikov.compiler.ProgramParser._
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.{BytePointer, Pointer, PointerPointer}

import scala.collection.JavaConversions._
import scala.collection.mutable

class CodegenProgramVisitor extends ProgramBaseVisitor[(String, LLVMValueRef)] {

  private val MAIN_METHOD_NAME = "main"
  private val MODULE_NAME = "module"

  private var module: LLVMModuleRef = null
  private var main: LLVMValueRef = null // only one function is supported for now
  private var entry: LLVMBasicBlockRef = null
  private var builder: LLVMBuilderRef = null
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

  override def visitProgram(ctx: ProgramContext): (String, LLVMValueRef) = {
    module = LLVMModuleCreateWithName(MODULE_NAME)
    main = initMain()
    entry = LLVMAppendBasicBlock(main, "entry")
    builder = LLVMCreateBuilder
    LLVMPositionBuilderAtEnd(builder, entry)
    initIO()
    variables = mutable.HashMap()
    ctx.functionDef.foreach((fDefCtx: FunctionDefContext) => visit(fDefCtx))

    LLVMBuildRetVoid(builder)
    LLVMDumpModule(module)
    val error = new BytePointer(null: Pointer)
    LLVMVerifyModule(module, LLVMAbortProcessAction, error)
    LLVMDisposeMessage(error)
    (null, null)
  }

  private def initMain(): LLVMValueRef = {
    val main = LLVMAddFunction(module, MAIN_METHOD_NAME, LLVMFunctionType(LLVMVoidType, LLVMVoidType, 0, 0))
    LLVMSetFunctionCallConv(main, LLVMCCallConv)
    main
  }

  override def visitBoolConst(ctx: BoolConstContext): (String, LLVMValueRef) = {
    val value = ctx.B.getSymbol.getText.toBoolean
    val valueRef = LLVMConstInt(LLVMInt1Type(), if (value) 1 else 0, 0)
    (Types.BOOLEAN, valueRef)
  }

  override def visitIntConst(ctx: IntConstContext): (String, LLVMValueRef) = {
    val value = ctx.Z.getSymbol.getText.toInt
    val valueRef = LLVMConstInt(LLVMInt32Type(), value, 0)
    (Types.INT, valueRef)
  }

  override def visitVarDecl(ctx: VarDeclContext): (String, LLVMValueRef) = {
    val typeName = ctx.ID(0).getSymbol.getText
    if (!List(Types.INT, Types.BOOLEAN).contains(typeName)) {
      throw new CompilationException(ctx, s"no such type: $typeName")
    }
    ctx.ID.subList(1, ctx.ID.size)
      .map { node => node.getSymbol.getText }
      .foreach { varName =>
        variables += (varName -> (typeName, null))
      }
    (null, null)
  }

  override def visitVariable(ctx: VariableContext): (String, LLVMValueRef) = {
    val varName = ctx.ID.getSymbol.getText
    checkVariableExists(varName, ctx)
    if (!variables.containsKey(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
    }
    val (typeName, value) = variables(varName)
    if (value == null) {
      throw new CompilationException(ctx, s"variable $varName is not initialized")
    }
    (typeName, value)
  }

  override def visitAssignmentExpr(ctx: AssignmentExprContext): (String, LLVMValueRef) = {
    val varName = ctx.ID.getSymbol.getText
    checkVariableExists(varName, ctx)
    val (typeName, _) = variables(varName)
    val (exprTypeName, value) = visit(ctx.expression)
    if (typeName != exprTypeName) {
      throw new CompilationException(ctx, s"incompatible types: ($typeName, $exprTypeName)")
    }
    variables(varName) = (typeName, value)
    (typeName, value)
  }

  def checkVariableExists(varName: String, ctx: ParserRuleContext): Unit = {
    if (!variables.containsKey(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
    }
  }

  override def visitFunctionCall(ctx: FunctionCallContext): (String, LLVMValueRef) = {
    val functionName = ctx.ID.getSymbol.getText
    val arguments = ctx.expressionList.expression.map((exprCtx) => visit(exprCtx))
    functionName match {
      case "print" =>
        if (arguments.size != 1) {
          throw new CompilationException(ctx, "wrong number of arguments")
        }
        val (_, value) = arguments.head
        val printfArgs = Array(numberFormat, value)
        val printf = LLVMGetNamedFunction(module, "printf")
        LLVMBuildCall(builder, printf, new PointerPointer(printfArgs:_*), printfArgs.length, generateName("print"))
    }
    (null, null)
  }

  override def visitSum(ctx: SumContext): (String, LLVMValueRef) = {
    val (leftType, left) = visit(ctx.expression(0))
    val (rightType, right) = visit(ctx.expression(1))
    if (leftType != Types.INT || rightType != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand types: ($leftType, $rightType)")
    }
    val result = ctx.SIGN.getSymbol.getText match {
      case "+" =>
        LLVMBuildAdd(builder, left, right, generateName("add"))
      case "-" =>
        LLVMBuildSub(builder, left, right, generateName("sub"))
    }
    (Types.INT, result)
  }

  override def visitMulDiv(ctx: MulDivContext): (String, LLVMValueRef) = {
    val (leftType, left) = visit(ctx.expression(0))
    val (rightType, right) = visit(ctx.expression(1))
    if (leftType != Types.INT || rightType != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand types: ($leftType, $rightType)")
    }
    val result = ctx.MULDIV.getSymbol.getText match {
      case "*" =>
        LLVMBuildMul(builder, left, right, generateName("mul"))
      case "/" =>
        LLVMBuildSDiv(builder, left, right, generateName("div"))
      case "%" =>
        LLVMBuildSRem(builder, left, right, generateName("mod"))
    }
    (Types.INT, result)
  }

  override def visitSignedExpr(ctx: SignedExprContext): (String, LLVMValueRef) = {
    val (typeName, value) = visit(ctx.expression)
    if (typeName != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand type: $typeName")
    }
    val resultValue = if (ctx.SIGN.getSymbol.getText == "-") {
      val zero = LLVMConstInt(LLVMInt32Type(), 0, 0)
      LLVMBuildSub(builder, zero, value, generateName("sub"))
    } else {
      value
    }
    (typeName, resultValue)
  }

  override def visitComparison(ctx: ComparisonContext): (String, LLVMValueRef) = {
    val operator: String = ctx.CMP().getSymbol.getText
    val (leftType, left) = visit(ctx.expression(0))
    val (rightType, right) = visit(ctx.expression(1))
    val typesConform = operator match {
      case "==" | "!=" =>
        (lt: String, rt: String) => lt == rt
      case _ =>
        (lt: String, rt: String) => lt == Types.INT && rt == Types.INT
    }
    if (!typesConform(leftType, rightType)) {
      throw new CompilationException(ctx, s"invalid operand types: ($leftType, $rightType)")
    }
    val predicate = operator match {
      case "==" => LLVMIntEQ
      case "!=" => LLVMIntNE
      case "<"  => LLVMIntSLT
      case "<=" => LLVMIntSLE
      case ">"  => LLVMIntSGT
      case ">=" => LLVMIntSGE
    }
    val result = LLVMBuildICmp(builder, predicate, left, right, generateName("cmp"))
    (Types.BOOLEAN, result)
  }

  override def visitJunction(ctx: JunctionContext): (String, LLVMValueRef) = {
    val (leftType, left) = visit(ctx.expression(0))
    val (rightType, right) = visit(ctx.expression(1))
    if (leftType != Types.INT || rightType != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand types: ($leftType, $rightType)")
    }
    val operator = ctx.JUNCTION.getSymbol.getText
    val result = operator match {
      case "&&" => LLVMBuildAnd(builder, left, right, generateName("and"))
      case "||" => LLVMBuildOr(builder, left, right, generateName("or"))
    }
    (Types.BOOLEAN, result)
  }
}
