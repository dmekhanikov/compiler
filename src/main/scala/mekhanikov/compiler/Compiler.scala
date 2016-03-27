package mekhanikov.compiler

import mekhanikov.compiler.ProgramParser.ProgramContext
import org.antlr.v4.runtime.tree.ParseTreeWalker
import org.antlr.v4.runtime.{ANTLRFileStream, ANTLRInputStream, CommonTokenStream}

object Compiler {

  def compile(fileName: String): Unit = {
    val tree = parse(fileName)
    val walker = new ParseTreeWalker()
    val listener = new CodegenProgramListener()
    walker.walk(listener, tree)
  }

  def parse(fileName: String): ProgramContext = {
    val input = new ANTLRFileStream(fileName)
    parse(input)
  }

  def parse(input: ANTLRInputStream): ProgramContext = {
    val lexer = new ProgramLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new ProgramParser(tokens)
    parser.program()
  }

  def main(args: Array[String]): Unit = {
    compile(args(0))
  }
}
