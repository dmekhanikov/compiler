package mekhanikov.compiler.statements

import mekhanikov.compiler.ProgramParser.WhileStmtContext
import mekhanikov.compiler._
import mekhanikov.compiler.types.Primitives
import org.bytedeco.javacpp.LLVM._

import scala.collection.mutable

class WhileStatements(val buildContext: BuildContext) {

  private val builder = buildContext.builder
  private val visitor = buildContext.visitor
  private val assignmentSearchVisitor = new AssignmentSearchVisitor(buildContext)

  def whileStatement(ctx: WhileStmtContext): Unit = {
    val currentFunction = buildContext.currentFunction.get
    val whileHead = LLVMAppendBasicBlock(currentFunction, "whileHead")
    val whileBody = LLVMAppendBasicBlock(currentFunction, "whileBody")
    val whileEnd = LLVMAppendBasicBlock(currentFunction, "whileEnd")
    val previousBlock = LLVMGetPreviousBasicBlock(whileHead)
    LLVMBuildBr(builder, whileHead)

    // find all assignments and replace variables' values with phi
    val assignedVarNames = assignmentSearchVisitor.visit(ctx)
    val phiVals = mutable.Map[String, LLVMValueRef]()
    LLVMPositionBuilderAtEnd(builder, whileHead)
    for (varName <- assignedVarNames) {
      val variable = buildContext.variables(varName)
      val phi = LLVMBuildPhi(builder, variable.varType.toLLVMType, "phi")
      // previous value of this variable
      LLVMAddIncoming(phi, variable.value.value, previousBlock, 1)
      buildContext.variables(varName).value = new Value(variable.varType, phi)
      phiVals(varName) = phi
    }

    // conditional jump
    val condition = visitor.visit(ctx.expression).get
    if (condition.valType != Primitives.BOOLEAN) {
      throw new CompilationException(ctx, "$condType type cannot be used in conditions")
    }
    LLVMBuildCondBr(builder, condition.value, whileBody, whileEnd)

    // loop body
    LLVMMoveBasicBlockAfter(whileBody, LLVMGetLastBasicBlock(currentFunction))
    LLVMPositionBuilderAtEnd(builder, whileBody)
    visitor.visit(ctx.block())
    LLVMBuildBr(builder, whileHead)

    // assign variables PHIs that where introduced in head
    LLVMMoveBasicBlockAfter(whileEnd, LLVMGetLastBasicBlock(currentFunction))
    LLVMPositionBuilderAtEnd(builder, whileEnd)
    val lastBodyBlock = LLVMGetPreviousBasicBlock(whileEnd)
    for (varName <- assignedVarNames) {
      val phi = phiVals(varName)
      val variable = buildContext.variables(varName)
      LLVMAddIncoming(phi, variable.value.value, lastBodyBlock, 1)
      buildContext.variables(varName).value = new Value(variable.varType, phi)
    }
  }
}
