package mekhanikov.compiler.statements

import mekhanikov.compiler.ProgramParser.IfStmtContext
import mekhanikov.compiler._
import mekhanikov.compiler.types.Primitives
import org.bytedeco.javacpp.LLVM._

import scala.collection.mutable

class IfStatements(val buildContext: BuildContext) {

  private val builder = buildContext.builder
  private val visitor = buildContext.visitor
  private val assignmentSearchVisitor = new AssignmentSearchVisitor(buildContext)

  def ifStatement(ctx: IfStmtContext): Unit = {
    val currentFunction = buildContext.currentFunction.get
    val thenBlock = LLVMAppendBasicBlock(currentFunction, "ifTrue")
    val elseBlock = LLVMAppendBasicBlock(currentFunction, "ifFalse")
    val endIf = LLVMAppendBasicBlock(currentFunction, "endIf")
    val condition = visitor.visit(ctx.expression).get
    if (condition.valType != Primitives.BOOLEAN) {
      throw new CompilationException(ctx, s"${condition.valType.name} type cannot be used in conditions")
    }
    LLVMBuildCondBr(builder, condition.value, thenBlock, elseBlock)

    // Variables changed in one branch may be used in another.
    // Their values should be restored before entering the else branch.
    // Also we need old values to make phi nodes
    val thenAssignedVarNames = assignmentSearchVisitor.visit(ctx.block(0))
    val elseAssignedVarNames = assignmentSearchVisitor.visit(ctx.block(1))
    val oldValues = mutable.Map[String, Value]()
    LLVMPositionBuilderAtEnd(builder, endIf)
    val phiVals = mutable.Map[String, LLVMValueRef]()
    for (name <- thenAssignedVarNames | elseAssignedVarNames) {
      val variable = buildContext.variables(name)
      oldValues(name) = variable.value
      val phi = LLVMBuildPhi(builder, variable.varType.toLLVMType, "phi")
      phiVals(name) = phi
    }

    // then branch
    LLVMMoveBasicBlockAfter(thenBlock, LLVMGetLastBasicBlock(currentFunction)) // ordering the blocks
    LLVMPositionBuilderAtEnd(builder, thenBlock)
    visitor.visit(ctx.block(0))
    LLVMBuildBr(builder, endIf)
    val lastThenBlock = LLVMGetLastBasicBlock(currentFunction)
    addIncomings(thenAssignedVarNames, lastThenBlock, phiVals)
    restoreValues(oldValues)
    addIncomings(elseAssignedVarNames &~ thenAssignedVarNames, lastThenBlock, phiVals)

    // else branch
    LLVMMoveBasicBlockAfter(elseBlock, LLVMGetLastBasicBlock(currentFunction)) // ordering the blocks
    LLVMPositionBuilderAtEnd(builder, elseBlock)
    visitor.visit(ctx.block(1))
    LLVMBuildBr(builder, endIf)
    val lastElseBlock = LLVMGetLastBasicBlock(currentFunction)
    addIncomings(elseAssignedVarNames, lastElseBlock, phiVals)
    restoreValues(oldValues)
    addIncomings(thenAssignedVarNames &~ elseAssignedVarNames, lastElseBlock, phiVals)

    // store phi nodes as variables' values
    for ((name, value) <- phiVals) {
      val variable = buildContext.variables(name)
      variable.value = new Value(variable.varType, value)
    }
    LLVMPositionBuilderAtEnd(builder, endIf)
    LLVMMoveBasicBlockAfter(endIf, LLVMGetLastBasicBlock(currentFunction))     // ordering the blocks
  }

  private def addIncomings(varNames: Set[String], block: LLVMBasicBlockRef, phiVals: mutable.Map[String, LLVMValueRef]): Unit = {
    for (varName <- varNames) {
      val phi = phiVals(varName)
      val value = buildContext.variables(varName).value.value
      LLVMAddIncoming(phi, value, block, 1)
    }
  }

  private def restoreValues(oldValues: mutable.Map[String, Value]): Unit = {
    oldValues.foreach { case (name, value) =>
      buildContext.variables(name).value = value
    }
  }
}
