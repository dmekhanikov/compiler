package mekhanikov.compiler.statements

import mekhanikov.compiler._
import mekhanikov.compiler.ProgramParser.WhileStmtContext
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
      val varTypeRef = Types.toTypeRef(variable.typeName)
      val phi = LLVMBuildPhi(builder, varTypeRef, "phi")
      // previous value of this variable
      if (variable.value.isDefined) {
        LLVMAddIncoming(phi, variable.value.get.value, previousBlock, 1)
      }
      buildContext.variables(varName).value = Some(new Value(variable.typeName, phi))
      phiVals(varName) = phi
    }

    // conditional jump
    val condition = visitor.visit(ctx.expression).get
    if (condition.typeName != Types.BOOLEAN) {
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
      LLVMAddIncoming(phi, variable.value.get.value, lastBodyBlock, 1)
      buildContext.variables(varName).value = Some(new Value(variable.typeName, phi))
    }
  }
}
