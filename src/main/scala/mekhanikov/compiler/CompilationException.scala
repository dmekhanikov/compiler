package mekhanikov.compiler

import org.antlr.v4.runtime.ParserRuleContext

class CompilationException(line: Int, pos: Int, message: String) extends Exception(s"<$line, $pos> $message") {

  def this(ctx: ParserRuleContext, message: String) {
    this(ctx.start.getLine, ctx.start.getCharPositionInLine, message)
  }
}
