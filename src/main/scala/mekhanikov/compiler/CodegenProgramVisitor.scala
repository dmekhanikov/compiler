package mekhanikov.compiler

import mekhanikov.compiler.ProgramParser._
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.{BytePointer, Pointer, PointerPointer}

import scala.collection.JavaConversions._
import scala.collection.mutable

// return type: (typeName, valueRef, Map[varName -> varValueRef])
// Map contains assigned variables in that block of code
class CodegenProgramVisitor extends ProgramBaseVisitor[(String, LLVMValueRef, Map[String, LLVMValueRef])] {

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

  override def visitProgram(ctx: ProgramContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
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
    null
  }

  private def initMain(): LLVMValueRef = {
    val main = LLVMAddFunction(module, MAIN_METHOD_NAME, LLVMFunctionType(LLVMVoidType, LLVMVoidType, 0, 0))
    LLVMSetFunctionCallConv(main, LLVMCCallConv)
    main
  }

  override def visitBoolConst(ctx: BoolConstContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val value = ctx.B.getSymbol.getText.toBoolean
    val valueRef = LLVMConstInt(LLVMInt1Type(), if (value) 1 else 0, 0)
    (Types.BOOLEAN, valueRef, Map())
  }

  override def visitIntConst(ctx: IntConstContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val value = ctx.Z.getSymbol.getText.toInt
    val valueRef = LLVMConstInt(LLVMInt32Type(), value, 0)
    (Types.INT, valueRef, Map())
  }

  override def visitVarDecl(ctx: VarDeclContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val typeName = ctx.ID(0).getSymbol.getText
    if (!List(Types.INT, Types.BOOLEAN).contains(typeName)) {
      throw new CompilationException(ctx, s"no such type: $typeName")
    }
    ctx.ID.subList(1, ctx.ID.size)
      .map { node => node.getSymbol.getText }
      .foreach { varName =>
        variables += (varName -> (typeName, null))
      }
    (null, null, Map())
  }

  override def visitVariable(ctx: VariableContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val varName = ctx.ID.getSymbol.getText
    checkVariableExists(varName, ctx)
    if (!variables.containsKey(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
    }
    val (typeName, value) = variables(varName)
    if (value == null) {
      throw new CompilationException(ctx, s"variable $varName is not initialized")
    }
    (typeName, value, Map())
  }

  override def visitAssignmentExpr(ctx: AssignmentExprContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val varName = ctx.ID.getSymbol.getText
    checkVariableExists(varName, ctx)
    val (typeName, _) = variables(varName)
    var (exprTypeName, value, assigned) = visit(ctx.expression)
    if (typeName != exprTypeName) {
      throw new CompilationException(ctx, s"incompatible types: ($typeName, $exprTypeName)")
    }
    variables(varName) = (typeName, value)
    assigned += (varName -> value)
    (typeName, value, assigned)
  }

  def checkVariableExists(varName: String, ctx: ParserRuleContext): Unit = {
    if (!variables.containsKey(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
    }
  }

  override def visitFunctionCall(ctx: FunctionCallContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val functionName = ctx.ID.getSymbol.getText
    val arguments = ctx.expressionList.expression.map((exprCtx) => visit(exprCtx))
    functionName match {
      case "print" =>
        if (arguments.size != 1) {
          throw new CompilationException(ctx, "wrong number of arguments")
        }
        val value = arguments.head._2
        val printfArgs = Array(numberFormat, value)
        val printf = LLVMGetNamedFunction(module, "printf")
        LLVMBuildCall(builder, printf, new PointerPointer(printfArgs:_*), printfArgs.length, generateName("print"))
    }
    val assigned = collectAssignments(arguments)
    (null, null, assigned)
  }

  override def visitSum(ctx: SumContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    var (leftType, left, leftAssigned) = visit(ctx.expression(0))
    val (rightType, right, rightAssigned) = visit(ctx.expression(1))
    if (leftType != Types.INT || rightType != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand types: ($leftType, $rightType)")
    }
    val result = ctx.SIGN.getSymbol.getText match {
      case "+" =>
        LLVMBuildAdd(builder, left, right, generateName("add"))
      case "-" =>
        LLVMBuildSub(builder, left, right, generateName("sub"))
    }
    leftAssigned ++= rightAssigned
    (Types.INT, result, leftAssigned)
  }

  override def visitMulDiv(ctx: MulDivContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    var (leftType, left, leftAssigned) = visit(ctx.expression(0))
    val (rightType, right, rightAssigned) = visit(ctx.expression(1))
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
    leftAssigned ++= rightAssigned
    (Types.INT, result, leftAssigned)
  }

  override def visitSignedExpr(ctx: SignedExprContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val (typeName, value, assigned) = visit(ctx.expression)
    if (typeName != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand type: $typeName")
    }
    val resultValue = if (ctx.SIGN.getSymbol.getText == "-") {
      val zero = LLVMConstInt(LLVMInt32Type(), 0, 0)
      LLVMBuildSub(builder, zero, value, generateName("sub"))
    } else {
      value
    }
    (typeName, resultValue, assigned)
  }

  override def visitComparison(ctx: ComparisonContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val operator: String = ctx.CMP().getSymbol.getText
    var (leftType, left, leftAssigned) = visit(ctx.expression(0))
    val (rightType, right, rightAssigned) = visit(ctx.expression(1))
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
    leftAssigned ++= rightAssigned
    (Types.BOOLEAN, result, leftAssigned)
  }

  override def visitJunction(ctx: JunctionContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    var (leftType, left, leftAssigned) = visit(ctx.expression(0))
    val (rightType, right, rightAssigned) = visit(ctx.expression(1))
    if (leftType != Types.INT || rightType != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand types: ($leftType, $rightType)")
    }
    val operator = ctx.JUNCTION.getSymbol.getText
    val result = operator match {
      case "&&" => LLVMBuildAnd(builder, left, right, generateName("and"))
      case "||" => LLVMBuildOr(builder, left, right, generateName("or"))
    }
    leftAssigned ++= rightAssigned
    (Types.BOOLEAN, result, leftAssigned)
  }

  override def visitIfStmt(ctx: IfStmtContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val thenBlock = LLVMAppendBasicBlock(main, generateName("ifTrue"))
    val elseBlock = LLVMAppendBasicBlock(main, generateName("ifFalse"))
    val endIf = LLVMAppendBasicBlock(main, generateName("endIf"))
    val (condType, condition, conditionAssigned) = visit(ctx.expression)
    if (condType != Types.BOOLEAN) {
      throw new CompilationException(ctx, s"$condType type cannot be used in conditions")
    }

    // then branch
    LLVMBuildCondBr(builder, condition, thenBlock, elseBlock)
    LLVMPositionBuilderAtEnd(builder, thenBlock)
    val thenAssigned = visit(ctx.block(0))._3
    LLVMBuildBr(builder, endIf)

    // else branch
    LLVMMoveBasicBlockAfter(elseBlock, LLVMGetLastBasicBlock(main)) // ordering the blocks
    LLVMPositionBuilderAtEnd(builder, elseBlock)
    val elseAssigned = visit(ctx.block(1))._3
    LLVMBuildBr(builder, endIf)

    LLVMMoveBasicBlockAfter(endIf, LLVMGetLastBasicBlock(main))     // ordering the blocks
    val lastThenBlock = LLVMGetPreviousBasicBlock(elseBlock)
    val lastElseBlock = LLVMGetPreviousBasicBlock(endIf)
    LLVMPositionBuilderAtEnd(builder, endIf)
    var totalAssigned = conditionAssigned ++ thenAssigned ++ elseAssigned
    for (varName <- thenAssigned.keySet & elseAssigned.keySet) {
      val varType = variables(varName)._1
      val varTypeRef = Types.toTypeRef(varType)
      val phi = LLVMBuildPhi(builder, varTypeRef, generateName("phi"))
      val thenValue = thenAssigned(varName)
      val elseValue = elseAssigned(varName)
      val phiVals = new PointerPointer(thenValue, elseValue)
      val phiBlocks = new PointerPointer(lastThenBlock, lastElseBlock)
      LLVMAddIncoming(phi, phiVals, phiBlocks, 2)
      variables += (varName -> (varType, phi))
      totalAssigned += (varName -> phi)
    }
    (null, null, totalAssigned)
  }

  override def visitWhileStmt(ctx: WhileStmtContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val whileHead = LLVMAppendBasicBlock(main, generateName("whileHead"))
    val whileBody = LLVMAppendBasicBlock(main, generateName("whileBody"))
    val whileEnd = LLVMAppendBasicBlock(main, generateName("whileEnd"))
    val previousBlock = LLVMGetPreviousBasicBlock(whileHead)
    LLVMBuildBr(builder, whileHead)

    // find all assignments and replace variables' values with phi
    val assignmentSearchVisitor = new AssignmentSearchVisitor()
    val assignedVarNames = assignmentSearchVisitor.visit(ctx)
    var phiVals = Map[String, LLVMValueRef]()
    LLVMPositionBuilderAtEnd(builder, whileHead)
    for (varName <- assignedVarNames) {
      val (typeName, oldValue) = variables(varName)
      val varTypeRef = Types.toTypeRef(typeName)
      val phi = LLVMBuildPhi(builder, varTypeRef, generateName("phi"))
      // previous value of this variable
      LLVMAddIncoming(phi, oldValue, previousBlock, 1)
      variables += (varName -> (typeName, phi))
      phiVals += (varName -> phi)
    }

    // conditional jump
    val (condType, condValue, condAssigned) = visit(ctx.expression)
    if (condType != Types.BOOLEAN) {
      throw new CompilationException(ctx, "$condType type cannot be used in conditions")
    }
    LLVMBuildCondBr(builder, condValue, whileBody, whileEnd)

    // loop body
    LLVMMoveBasicBlockAfter(whileBody, LLVMGetLastBasicBlock(main))
    LLVMPositionBuilderAtEnd(builder, whileBody)
    val (_, _, assigned) = visit(ctx.block())
    LLVMBuildBr(builder, whileHead)

    // assign variables PHIs that where introduced in head
    LLVMMoveBasicBlockAfter(whileEnd, LLVMGetLastBasicBlock(main))
    LLVMPositionBuilderAtEnd(builder, whileEnd)
    val lastBodyBlock = LLVMGetPreviousBasicBlock(whileEnd)
    for (varName <- assignedVarNames) {
      val phi = phiVals(varName)
      val (typeName, newValue) = variables(varName)
      LLVMAddIncoming(phi, newValue, lastBodyBlock, 1)
      variables(varName) = (typeName, phi)
    }

    (null, null, condAssigned ++ assigned)
  }

  override def visitBlock(ctx: BlockContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    val results = ctx.statement.map(stmt => visit(stmt))
    val assigned = collectAssignments(results)
    (null, null, assigned)
  }

  override def visitExprStmt(ctx: ExprStmtContext): (String, LLVMValueRef, Map[String, LLVMValueRef]) = {
    visit(ctx.expression)
  }

  private def collectAssignments(results: Seq[(String, LLVMValueRef, Map[String, LLVMValueRef])]): Map[String, LLVMValueRef] = {
    val assignmentsList = results.map(result => result._3)
    assignmentsList.foldLeft(Map[String, LLVMValueRef]()) {(a, r) => r ++ a}
  }
}
