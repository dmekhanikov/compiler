package mekhanikov.compiler

import org.antlr.v4.runtime.ParserRuleContext

class CompilationException(message: String) extends Exception(message) {

  def this(line: Int, pos: Int, message: String) {
    this(s"<$line, $pos> $message")
  }

  def this(ctx: ParserRuleContext, message: String) {
    this(ctx.start.getLine, ctx.start.getCharPositionInLine, message)
  }
}
