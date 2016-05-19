package mekhanikov.compiler

import mekhanikov.compiler.ProgramParser._
import mekhanikov.compiler.definitions.{FunctionDefinitions, Structures, VariableDeclarations}
import mekhanikov.compiler.expressions._
import mekhanikov.compiler.statements.WhileStatements

import scala.collection.JavaConversions._

class CodegenProgramVisitor extends ProgramBaseVisitor[Option[Value]] {

  val buildContext = new BuildContext(this)
  private val constants = new Constants(buildContext)
  private val variables = new Variables(buildContext)
  private val ariphmetics = new Ariphmetics(buildContext)
  private val functionCalls = new FunctionCalls(buildContext)
  private val condExpr = new CondExpr(buildContext)
  private val whileStatements = new WhileStatements(buildContext)
  private val variableDeclarations = new VariableDeclarations(buildContext)
  private val functionDefinitions = new FunctionDefinitions(buildContext)
  private val structures = new Structures(buildContext, functionDefinitions, functionCalls)

  override def visitProgram(ctx: ProgramContext): Option[Value] = {
    ctx.children.foreach(childCtx => visit(childCtx))
    None
  }

  override def visitFunctionDef(ctx: FunctionDefContext): Option[Value] = {
    functionDefinitions.function(ctx)
    None
  }

  override def visitStructDef(ctx: StructDefContext): Option[Value] = {
    structures.struct(ctx)
    None
  }

  override def visitNewExpr(ctx: NewExprContext): Option[Value] =
    Some(structures.newExpr(ctx))

  override def visitFieldRead(ctx: FieldReadContext): Option[Value] =
    Some(structures.readAccess(ctx))

  override def visitFieldWrite(ctx: FieldWriteContext): Option[Value] =
    Some(structures.writeAccess(ctx))


  override def visitMethodCall(ctx: MethodCallContext): Option[Value] =
    Some(structures.methodCall(ctx))

  override def visitBoolConst(ctx: BoolConstContext): Option[Value] =
    Some(constants.bool(ctx))

  override def visitIntConst(ctx: IntConstContext): Option[Value] =
    Some(constants.int(ctx))

  override def visitVarDecl(ctx: VarDeclContext): Option[Value] = {
    variableDeclarations.variable(ctx)
    None
  }

  override def visitVariable(ctx: VariableContext): Option[Value] =
    Some(variables.variable(ctx))

  override def visitVarAssignment(ctx: VarAssignmentContext): Option[Value] =
    Some(variables.assignment(ctx))

  override def visitFunctionCall(ctx: FunctionCallContext): Option[Value] =
    Some(functionCalls.call(ctx))

  override def visitSum(ctx: SumContext): Option[Value] =
    Some(ariphmetics.sum(ctx))

  override def visitMulDiv(ctx: MulDivContext): Option[Value] =
    Some(ariphmetics.muldiv(ctx))

  override def visitSignedExpr(ctx: SignedExprContext): Option[Value] =
    Some(ariphmetics.sign(ctx))

  override def visitParens(ctx: ParensContext): Option[Value] = {
    visit(ctx.expression)
  }

  override def visitComparison(ctx: ComparisonContext): Option[Value] =
    Some(ariphmetics.comparison(ctx))

  override def visitJunction(ctx: JunctionContext): Option[Value] =
    Some(ariphmetics.junction(ctx))


  override def visitCondExpr(ctx: CondExprContext): Option[Value] = {
    Some(condExpr.condExpr(ctx))
  }

  override def visitWhileStmt(ctx: WhileStmtContext): Option[Value] = {
    whileStatements.whileStatement(ctx)
    None
  }

  override def visitExprStmt(ctx: ExprStmtContext): Option[Value] = {
    visit(ctx.expression)
    None
  }

  override def visitBlock(ctx: BlockContext): Option[Value] = {
    ctx.statement.foreach(stmt => visit(stmt))
    if (Option(ctx.expression).isDefined) {
      visit(ctx.expression)
    } else {
      None
    }
  }
}
