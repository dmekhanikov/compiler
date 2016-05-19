package mekhanikov.compiler

import mekhanikov.compiler.ProgramParser.ProgramContext
import org.antlr.v4.runtime.{ANTLRFileStream, ANTLRInputStream, CommonTokenStream}
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.{BytePointer, Pointer}

object Compiler {

  def generateModule(ast:ProgramContext): LLVMModuleRef = {
    val visitor = new CodegenProgramVisitor()
    visitor.visit(ast)
    val error = new BytePointer(null: Pointer)
    val module = visitor.buildContext.module
    LLVMVerifyModule(module, LLVMAbortProcessAction, error)
    LLVMDisposeMessage(error)
    module
  }

  def compile(module: LLVMModuleRef): LLVMExecutionEngineRef = {
    LLVMLinkInMCJIT()
    LLVMInitializeNativeAsmPrinter()
    LLVMInitializeNativeAsmParser()
    LLVMInitializeNativeDisassembler()
    LLVMInitializeNativeTarget()
    val engine: LLVMExecutionEngineRef = new LLVMExecutionEngineRef
    val provider: LLVMModuleProviderRef = LLVMCreateModuleProviderForExistingModule(module)
    val error = new BytePointer(null.asInstanceOf[Pointer])
    if (LLVMCreateJITCompiler(engine, provider, 2, error) != 0) {
      val message = error.getString
      LLVMDisposeMessage(error)
      throw new IllegalStateException(message)
    }
    engine
  }

  def parse(fileName: String): ProgramContext = {
    val input = new ANTLRFileStream(fileName)
    parse(input)
  }

  def parse(input: ANTLRInputStream): ProgramContext = {
    val lexer = new ProgramLexer(input)
    val tokens = new CommonTokenStream(lexer)
    val parser = new ProgramParser(tokens)
    val programContext = parser.program()
    val errors = parser.getNumberOfSyntaxErrors
    if (errors != 0) {
      throw new CompilationException(s"There are $errors syntax errors")
    }
    programContext
  }

  def main(args: Array[String]): Unit = {
    val ast = parse(args(0))
    val module = generateModule(ast)
    LLVMDumpModule(module)
  }
}
