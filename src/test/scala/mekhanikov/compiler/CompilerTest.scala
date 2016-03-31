package mekhanikov.compiler

import org.antlr.v4.runtime.ANTLRInputStream
import org.bytedeco.javacpp.LLVM._
import org.bytedeco.javacpp.PointerPointer
import org.junit.Assert._
import org.junit.Test

class CompilerTest {

  private def runTest(src: String, expected: Int): Unit = {
    val ast = Compiler.parse(new ANTLRInputStream(src))
    val module = Compiler.generateModule(ast)
    val execEngine = Compiler.compile(module)
    val function = LLVMGetNamedFunction(module, "box")
    val result = LLVMRunFunction(execEngine, function, 0, new PointerPointer())
    val actual = LLVMGenericValueToInt(result, 0)
    assertEquals(expected, actual)
  }

  private def expectException(exceptionClass: Class[_ <: Exception]) (code: () => Unit): Unit = {
    try {
      code()
      throw new AssertionError("No exceptions were thrown")
    } catch {
      case e: Throwable =>
        if (e.getClass != exceptionClass) {
          throw e
        }
    }
  }

  private def expectSyntaxException(src: String): Unit = {
    expectException(classOf[CompilationException]) {() => Compiler.parse(new ANTLRInputStream(src)) }
  }

  private def expectSemanticException(src: String): Unit = {
    val ast = Compiler.parse(new ANTLRInputStream(src))
    expectException(classOf[CompilationException]) {() =>
      Compiler.generateModule(ast)
    }
  }

  @Test
  def arithmetic(): Unit = {
    val src =
      """int box() {
        |    int a, b, c;
        |    a = 5;
        |    b = 4;
        |    c = 3;
        |    return a * (b + c) - a * 20 / b + c;
        |}
      """.stripMargin
    runTest(src, 13)
  }

  @Test
  def boolean(): Unit = {
    val src1 =
      """bool box() {
        |    return (4 < 5) && false || (15 + 3 == 18);
        |}
      """.stripMargin
    runTest(src1, 1)
    val src2 =
      """bool box() {
        |    return false && (false || true);
        |}
      """.stripMargin
    runTest(src2, 0)
  }

  @Test
  def conditions(): Unit = {
    val src =
      """int box() {
        |    int a, b;
        |    a = 5;
        |    if (a % 2 == 0) {
        |        b = 1;
        |    } else {
        |        b = 2;
        |    }
        |    return b;
        |}
      """.stripMargin
    runTest(src, 2)
  }

  @Test
  def loops(): Unit = {
    val src =
      """int box() {
        |    int x, y, res;
        |    x = 2;
        |    y = 8;
        |    res = 1;
        |    while (y > 0) {
        |        res = res * x;
        |        y = y - 1;
        |    }
        |    return res;
        |}
      """.stripMargin
    runTest(src, 256)
  }

  @Test
  def complexConstructions(): Unit = {
    val src =
      """int box() {
        |    int a, b, c;
        |    a = 5;
        |    b = 6;
        |    c = 7;
        |    while (a > 1) {
        |        if (a > 27) {
        |            while (true) {}
        |            a = -2;
        |            b = 8;
        |        } else {
        |            a = a - 1;
        |            c = c + 1;
        |        }
        |    }
        |    return a + b + c;
        |}
      """.stripMargin
    runTest(src, 18)
  }

  @Test
  def functionCall(): Unit = {
    val src =
      """int mul(int x, int y) { return x * y; }
        |int box() {
        |    return mul(5, 12);
        |}
      """.stripMargin
    runTest(src, 60)
  }

  @Test
  def missingSemicolon(): Unit = {
    expectSyntaxException("void box() { return }")
    expectSyntaxException(
      """int box() {
        |    int a, b;
        |    a = 1
        |    b = 2;
        |    return a + b;
        |}
      """.stripMargin)
    expectSyntaxException(
      """int box() {
        |    int a, b
        |    a = 1;
        |    b = 2;
        |    return a + b;
        |}
      """.stripMargin)
  }

  @Test
  def missingBracket(): Unit = {
    expectSyntaxException("void box() { return;")
  }

  @Test
  def missingReturn(): Unit = {
    expectSyntaxException(
      """void box() {
        |    int a;
        |    a = 1;
        |    b = 2;
        |}
      """.stripMargin)
  }

  @Test
  def undeclaredVariable(): Unit = {
    expectSemanticException(
      """int box() {
        |    int a;
        |    a = 1;
        |    b = 2;
        |    return a + b;
        |}
      """.stripMargin)
  }

  @Test
  def undefinedFunction(): Unit = {
    expectSemanticException(
      """int box() {
        |    return mul(2, 2);
        |}
      """.stripMargin)
    expectSemanticException(
      """int box() {
        |    return mul(2, 2);
        |}
        |int mul(int x, int y) { return x * y; }
      """.stripMargin)
    expectSemanticException(
      """int mul(int x) { return x * x; }
        |int box() {
        |    return mul(2, 2);
        |}
      """.stripMargin)
    expectSemanticException(
      """int mul(int x, bool b) { return x; }
        |int box() {
        |    return mul(2, 2);
        |}
      """.stripMargin)
  }

  @Test
  def functionRedeclaration(): Unit = {
    expectSemanticException(
      """int box(int x, int y) { return x; }
        |int box(int x, int y) { return y; }
      """.stripMargin)
    expectSemanticException(
      """int box(int x, int y) { return x; }
        |int box(int x, bool y) { return x; }
      """.stripMargin)
    expectSemanticException(
      """int box(int x, int y) { return x; }
        |int box(int x) { return x; }
      """.stripMargin)
    expectSemanticException(
      """int box(int x, int y) { return x; }
        |bool box(int x, int y) { return true; }
      """.stripMargin)
  }

  @Test
  def typeMismatch(): Unit = {
    expectSemanticException("int box() { return true; }")
    expectSemanticException("bool box() { return 1; }")
    expectSemanticException("bool box() { return; }")
    expectSemanticException("void box() { return 1; }")
    expectSemanticException(
      """bool box() {
        |    int a;
        |    a = 5;
        |    return a;
        |}
      """.stripMargin)
    expectSemanticException(
      """void box() {
        |    int a;
        |    a = true;
        |    return;
        |}
      """.stripMargin)
    expectSemanticException(
      """void box() {
        |    bool a;
        |    a = 42;
        |    return;
        |}
      """.stripMargin)
    expectSemanticException(
      """int dup(int x) { return x * 2; }
        |bool box() {
        |   return dup(2);
        |}
      """.stripMargin)
  }

  @Test
  def uninitializedVariable(): Unit = {
    expectSemanticException(
      """int box() {
        |    int a, b;
        |    a = b + 2;
        |    return a;
        |}
      """.stripMargin)
  }
}
