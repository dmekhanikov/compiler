package mekhanikov.compiler

import mekhanikov.compiler.ProgramParser._
import org.antlr.v4.runtime.ParserRuleContext
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.{BytePointer, Pointer, PointerPointer}

import scala.collection.JavaConversions._
import scala.collection.mutable

class CodegenProgramVisitor extends ProgramBaseVisitor[(String, LLVMValueRef)] {

  private val MODULE_NAME = "module"

  private var _module: LLVMModuleRef = null
  def module = _module
  private var builder: LLVMBuilderRef = null
  private var functionSignatures: mutable.HashMap[String, (String, List[String])] = null
  private var currentFunction: LLVMValueRef = null
  private var curReturnType: String = null
  private var localVariables: mutable.HashMap[String, (String, LLVMValueRef)] = null  // name -> (typename, value)
  private val assignmentSearchVisitor = new AssignmentSearchVisitor()

  private var numberFormat: LLVMValueRef = null

  private var genIndex = 0

  private def declarePrintf(): Unit = {
    val i8Pointer = LLVMPointerType(LLVMInt8Type(), 0)
    val fnType = LLVMFunctionType(LLVMInt32Type(), i8Pointer, 1, 1)
    val fn = LLVMAddFunction(module, "printf", fnType)
    LLVMSetFunctionCallConv(fn, LLVMCCallConv)
  }

  override def visitProgram(ctx: ProgramContext): (String, LLVMValueRef) = {
    _module = LLVMModuleCreateWithName(MODULE_NAME)
    builder = LLVMCreateBuilder
    declarePrintf()
    functionSignatures = mutable.HashMap()
    ctx.functionDef.foreach((fDefCtx: FunctionDefContext) => visit(fDefCtx))
    null
  }

  override def visitFunctionDef(ctx: FunctionDefContext): (String, LLVMValueRef) = {
    curReturnType = ctx.ID(0).getText
    val functionName = ctx.ID(1).getText
    if (LLVMGetNamedFunction(module, functionName) != null) {
      throw new CompilationException(ctx, "function with this name already exists")
    }
    val parameterList = ctx.parameterList
    val argTypes = if (parameterList != null) {
      parameterList.parameter.map {parCtx => parCtx.ID(0).getText}.toList
    } else {
      List()
    }
    currentFunction = createFunction(functionName, curReturnType, argTypes)
    functionSignatures += (functionName -> (curReturnType, argTypes))
    localVariables = mutable.HashMap()
    if (parameterList != null) {
      for ((parCtx, i) <- parameterList.parameter.zipWithIndex) {
        val typeName = parCtx.ID(0).getText
        val varName = parCtx.ID(1).getText
        val value = LLVMGetParam(currentFunction, i)
        localVariables += (varName -> (typeName, value))
      }
    }
    ctx.varDecl.foreach(visit)
    ctx.statement.foreach(visit)
    buildReturn(ctx)
    null
  }

  private def buildReturn(ctx: FunctionDefContext): Unit = {
    val returnExpr = ctx.expression
    if (returnExpr == null) {
      if (curReturnType == Types.VOID) {
        LLVMBuildRetVoid(builder)
      } else {
        throw new CompilationException(ctx, "cannot return void from this function")
      }
    } else {
      val (typeName, value) = visit(returnExpr)
      if (typeName != curReturnType) {
        throw new CompilationException(ctx, s"expecting expression of type $curReturnType, but was: $typeName")
      } else {
        LLVMBuildRet(builder, value)
      }
    }
  }

  private def createFunction(name: String, returnType: String, argTypes: Seq[String]): LLVMValueRef = {
    val returnTypeRef = Types.toTypeRef(returnType)
    val argTypeRefs = new PointerPointer(argTypes.map(typeName => Types.toTypeRef(typeName)):_*)
    val function = LLVMAddFunction(module, name, LLVMFunctionType(returnTypeRef, argTypeRefs, argTypes.size, 0))
    LLVMSetFunctionCallConv(function, LLVMCCallConv)
    val entry = LLVMAppendBasicBlock(function, "entry")
    LLVMPositionBuilderAtEnd(builder, entry)
    function
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
        if (localVariables.contains(varName)) {
          throw new CompilationException(ctx, s"repeated declaration of the variable $varName")
        }
        localVariables += (varName -> (typeName, null))
      }
    (null, null)
  }

  override def visitVariable(ctx: VariableContext): (String, LLVMValueRef) = {
    val varName = ctx.ID.getSymbol.getText
    checkVariableExists(varName, ctx)
    if (!localVariables.containsKey(varName)) {
      throw new CompilationException(ctx, s"variable $varName is not defined")
    }
    val (typeName, value) = localVariables(varName)
    if (value == null) {
      throw new CompilationException(ctx, s"variable $varName is not initialized")
    }
    (typeName, value)
  }

  override def visitAssignmentExpr(ctx: AssignmentExprContext): (String, LLVMValueRef) = {
    val varName = ctx.ID.getSymbol.getText
    checkVariableExists(varName, ctx)
    val (typeName, _) = localVariables(varName)
    val (exprTypeName, value) = visit(ctx.expression)
    if (typeName != exprTypeName) {
      throw new CompilationException(ctx, s"incompatible types: ($typeName, $exprTypeName)")
    }
    localVariables(varName) = (typeName, value)
    (typeName, value)
  }

  def checkVariableExists(varName: String, ctx: ParserRuleContext): Unit = {
    if (!localVariables.containsKey(varName)) {
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
        val value = arguments.head._2
        if (numberFormat == null) {
          numberFormat = LLVMBuildGlobalStringPtr(builder, "%d\n", "numberFormat")
        }
        val printfArgs = Array(numberFormat, value)
        val printf = LLVMGetNamedFunction(module, "printf")
        LLVMBuildCall(builder, printf, new PointerPointer(printfArgs:_*), printfArgs.length, "print")
        (Types.VOID, null)
      case _ =>
        val function = LLVMGetNamedFunction(module, functionName)
        if (function == null) {
          throw new CompilationException(ctx, "call to an undeclared function")
        }
        val (retType, argTypes) = functionSignatures(functionName)
        val argsExpr = ctx.expressionList
        val args = if (argsExpr != null) {
          if (argsExpr.expression.size != argTypes.size) {
            throw new CompilationException(ctx, "wrong number of arguments")
          }
          val providedArgs = argsExpr.expression.foldRight(List[(String, LLVMValueRef)]()) { (exprCtx, r) =>
            visit(exprCtx) :: r
          }
          val providedArgTypes = providedArgs.map(arg => arg._1)
          if (providedArgTypes != argTypes) {
            throw new CompilationException(ctx, "wrong function signature")
          }
          providedArgs.map(arg => arg._2)
        } else {
          List()
        }
        val callRes = LLVMBuildCall(builder, function, new PointerPointer(args:_*), args.size, "call")
        (retType, callRes)
    }
  }

  override def visitSum(ctx: SumContext): (String, LLVMValueRef) = {
    val (leftType, left) = visit(ctx.expression(0))
    val (rightType, right) = visit(ctx.expression(1))
    if (leftType != Types.INT || rightType != Types.INT) {
      throw new CompilationException(ctx, s"invalid operand types: ($leftType, $rightType)")
    }
    val result = ctx.SIGN.getSymbol.getText match {
      case "+" =>
        LLVMBuildAdd(builder, left, right, "add")
      case "-" =>
        LLVMBuildSub(builder, left, right, "sub")
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
        LLVMBuildMul(builder, left, right, "mul")
      case "/" =>
        LLVMBuildSDiv(builder, left, right, "div")
      case "%" =>
        LLVMBuildSRem(builder, left, right, "mod")
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
      LLVMBuildSub(builder, zero, value, "sub")
    } else {
      value
    }
    (typeName, resultValue)
  }


  override def visitParens(ctx: ParensContext): (String, LLVMValueRef) = {
    visit(ctx.expression)
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
    val result = LLVMBuildICmp(builder, predicate, left, right, "cmp")
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
      case "&&" => LLVMBuildAnd(builder, left, right, "and")
      case "||" => LLVMBuildOr(builder, left, right, "or")
    }
    (Types.BOOLEAN, result)
  }

  override def visitIfStmt(ctx: IfStmtContext): (String, LLVMValueRef) = {
    val thenBlock = LLVMAppendBasicBlock(currentFunction, "ifTrue")
    val elseBlock = LLVMAppendBasicBlock(currentFunction, "ifFalse")
    val endIf = LLVMAppendBasicBlock(currentFunction, "endIf")
    val (condType, condition) = visit(ctx.expression)
    if (condType != Types.BOOLEAN) {
      throw new CompilationException(ctx, s"$condType type cannot be used in conditions")
    }
    LLVMBuildCondBr(builder, condition, thenBlock, elseBlock)

    // Variables changed in one branch may be used in another.
    // Their values should be restored before entering the else branch.
    // Also we need old values to make phi nodes
    val thenAssignedVarNames = assignmentSearchVisitor.visit(ctx.block(0))
    val elseAssignedVarNames = assignmentSearchVisitor.visit(ctx.block(1))
    var oldValues = Map[String, (String, LLVMValueRef)]()
    LLVMPositionBuilderAtEnd(builder, endIf)
    var phiVals = Map[String, LLVMValueRef]()
    for (name <- thenAssignedVarNames | elseAssignedVarNames) {
      val (typeName, oldValue) = localVariables(name)
      oldValues += (name -> (typeName, oldValue))
      val phi = LLVMBuildPhi(builder, Types.toTypeRef(typeName), "phi")
      phiVals += (name -> phi)
    }

    // then branch
    LLVMMoveBasicBlockAfter(thenBlock, LLVMGetLastBasicBlock(currentFunction)) // ordering the blocks
    LLVMPositionBuilderAtEnd(builder, thenBlock)
    visit(ctx.block(0))
    LLVMBuildBr(builder, endIf)
    val lastThenBlock = LLVMGetLastBasicBlock(currentFunction)
    addIncomings(thenAssignedVarNames, lastThenBlock, phiVals)
    localVariables ++= oldValues
    addIncomings(elseAssignedVarNames &~ thenAssignedVarNames, lastThenBlock, phiVals)

    // else branch
    LLVMMoveBasicBlockAfter(elseBlock, LLVMGetLastBasicBlock(currentFunction)) // ordering the blocks
    LLVMPositionBuilderAtEnd(builder, elseBlock)
    visit(ctx.block(1))
    LLVMBuildBr(builder, endIf)
    val lastElseBlock = LLVMGetLastBasicBlock(currentFunction)
    addIncomings(elseAssignedVarNames, lastElseBlock, phiVals)
    localVariables ++= oldValues
    addIncomings(thenAssignedVarNames &~ elseAssignedVarNames, lastElseBlock, phiVals)

    // store phi nodes as variables' values
    for ((name, value) <- phiVals) {
      val typeName = localVariables(name)._1
      localVariables += (name -> (typeName, value))
    }
    LLVMPositionBuilderAtEnd(builder, endIf)
    LLVMMoveBasicBlockAfter(endIf, LLVMGetLastBasicBlock(currentFunction))     // ordering the blocks
    (null, null)
  }

  private def addIncomings(varNames: Set[String], block: LLVMBasicBlockRef, phiVals: Map[String, LLVMValueRef]): Unit = {
    for (varName <- varNames) {
      val phi = phiVals(varName)
      LLVMAddIncoming(phi, localVariables(varName)._2, block, 1)
    }
  }

  override def visitWhileStmt(ctx: WhileStmtContext): (String, LLVMValueRef) = {
    val whileHead = LLVMAppendBasicBlock(currentFunction, "whileHead")
    val whileBody = LLVMAppendBasicBlock(currentFunction, "whileBody")
    val whileEnd = LLVMAppendBasicBlock(currentFunction, "whileEnd")
    val previousBlock = LLVMGetPreviousBasicBlock(whileHead)
    LLVMBuildBr(builder, whileHead)

    // find all assignments and replace variables' values with phi
    val assignedVarNames = assignmentSearchVisitor.visit(ctx)
    var phiVals = Map[String, LLVMValueRef]()
    LLVMPositionBuilderAtEnd(builder, whileHead)
    for (varName <- assignedVarNames) {
      val (typeName, oldValue) = localVariables(varName)
      val varTypeRef = Types.toTypeRef(typeName)
      val phi = LLVMBuildPhi(builder, varTypeRef, "phi")
      // previous value of this variable
      LLVMAddIncoming(phi, oldValue, previousBlock, 1)
      localVariables += (varName -> (typeName, phi))
      phiVals += (varName -> phi)
    }

    // conditional jump
    val (condType, condValue) = visit(ctx.expression)
    if (condType != Types.BOOLEAN) {
      throw new CompilationException(ctx, "$condType type cannot be used in conditions")
    }
    LLVMBuildCondBr(builder, condValue, whileBody, whileEnd)

    // loop body
    LLVMMoveBasicBlockAfter(whileBody, LLVMGetLastBasicBlock(currentFunction))
    LLVMPositionBuilderAtEnd(builder, whileBody)
    visit(ctx.block())
    LLVMBuildBr(builder, whileHead)

    // assign variables PHIs that where introduced in head
    LLVMMoveBasicBlockAfter(whileEnd, LLVMGetLastBasicBlock(currentFunction))
    LLVMPositionBuilderAtEnd(builder, whileEnd)
    val lastBodyBlock = LLVMGetPreviousBasicBlock(whileEnd)
    for (varName <- assignedVarNames) {
      val phi = phiVals(varName)
      val (typeName, newValue) = localVariables(varName)
      LLVMAddIncoming(phi, newValue, lastBodyBlock, 1)
      localVariables(varName) = (typeName, phi)
    }

    (null, null)
  }

  override def visitExprStmt(ctx: ExprStmtContext): (String, LLVMValueRef) = {
    visit(ctx.expression)
  }

  override def visitBlock(ctx: BlockContext): (String, LLVMValueRef) = {
    ctx.statement.foreach(stmt => visit(stmt))
    (null, null)
  }
}
