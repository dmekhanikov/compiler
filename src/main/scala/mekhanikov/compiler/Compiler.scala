package mekhanikov.compiler

import mekhanikov.compiler.ProgramParser.ProgramContext
import org.antlr.v4.runtime.{ANTLRFileStream, ANTLRInputStream, CommonTokenStream}
import org.bytedeco.javacpp.{BytePointer, Pointer}
import org.bytedeco.javacpp.LLVM._

object Compiler {

  def compile(fileName: String): Unit = {
    val tree = parse(fileName)
    val visitor = new CodegenProgramVisitor()
    visitor.visit(tree)
    LLVMDumpModule(visitor.module)
    val error = new BytePointer(null: Pointer)
    LLVMVerifyModule(visitor.module, LLVMAbortProcessAction, error)
    LLVMDisposeMessage(error)
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
