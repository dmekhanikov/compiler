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
        |    a * (b + c) - a * 20 / b + c
        |}
      """.stripMargin
    runTest(src, 13)
  }

  @Test
  def boolean(): Unit = {
    val src1 =
      """bool box() {
        |    (4 < 5) && false || (15 + 3 == 18)
        |}
      """.stripMargin
    runTest(src1, 1)
    val src2 =
      """bool box() {
        |    false && (false || true)
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
        |    };
        |    b
        |}
      """.stripMargin
    runTest(src, 2)
  }

  @Test
  def conditionalReturn(): Unit = {
    val src =
      """int fact(int x) {
        |   if (x == 0) {
        |     1
        |   } else {
        |     x * fact(x - 1)
        |   }
        |}
        |int box() {
        |   fact(6)
        |}
      """.stripMargin
    runTest(src, 720)
  }

  @Test
  def conditionalAssignment(): Unit = {
    val src =
      """int box() {
        |   int a = if (true) { 1 } else { 2 };
        |   a
        |}
      """.stripMargin
    runTest(src, 1)
  }

  @Test
  def conditionalTypeInference(): Unit = {
    val src =
      """struct A { int a; }
        |struct B : A {}
        |struct C : B {}
        |struct C1 : C {}
        |struct D : B {}
        |int box() {
        |   C1 c = new C1();
        |   D d = new D();
        |   A x;
        |   c.a = 1;
        |   d.a = 2;
        |   x = if (true) {
        |     c
        |   } else {
        |     d
        |   };
        |   x.a
        |}
      """.stripMargin
    runTest(src, 1)
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
        |    res
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
        |    a + b + c
        |}
      """.stripMargin
    runTest(src, 18)
  }

  @Test
  def functionCall(): Unit = {
    val src =
      """int mul(int x, int y) { x * y }
        |int box() {
        |    mul(5, 12)
        |}
      """.stripMargin
    runTest(src, 60)
  }

  @Test
  def voidFunction(): Unit = {
    val src =
      """void foo() { }
        |int box() {
        |   foo();
        |   42
        |}
      """.stripMargin
    runTest(src, 42)
  }

  @Test
  def missingSemicolon(): Unit = {
    expectSyntaxException(
      """int box() {
        |    int a, b;
        |    a = 1
        |    b = 2;
        |    a + b
        |}
      """.stripMargin)
    expectSyntaxException(
      """int box() {
        |    int a, b
        |    a = 1;
        |    b = 2;
        |    a + b
        |}
      """.stripMargin)
  }

  @Test
  def missingBracket(): Unit = {
    expectSyntaxException("void box() { ")
  }

  @Test
  def missingReturnValue(): Unit = {
    expectSemanticException(
      """int box() {
        |    int a;
        |    a = 1;
        |    b = 2;
        |}
      """.stripMargin)
  }

  @Test
  def noVoidReturn(): Unit = {
    val src =
      """struct A { int a; }
        |void setA(A a, int val) { a.a = val; }
        |int box() {
        |   A a = new A();
        |   setA(a, 5);
        |   a.a
        |}
      """.stripMargin
    runTest(src, 5)
  }

  @Test
  def undeclaredVariable(): Unit = {
    expectSemanticException(
      """int box() {
        |    int a;
        |    a = 1;
        |    b = 2;
        |    a + b
        |}
      """.stripMargin)
  }

  @Test
  def undefinedFunction(): Unit = {
    expectSemanticException(
      """int box() {
        |    mul(2, 2)
        |}
      """.stripMargin)
    expectSemanticException(
      """int box() {
        |    mul(2, 2)
        |}
        |int mul(int x, int y) { x * y }
      """.stripMargin)
    expectSemanticException(
      """int mul(int x) { x * x }
        |int box() {
        |    mul(2, 2)
        |}
      """.stripMargin)
    expectSemanticException(
      """int mul(int x, bool b) { x }
        |int box() {
        |    mul(2, 2)
        |}
      """.stripMargin)
  }

  @Test
  def functionRedeclaration(): Unit = {
    expectSemanticException(
      """int box(int x, int y) { x }
        |int box(int x, int y) { y }
      """.stripMargin)
    expectSemanticException(
      """int box(int x, int y) { x }
        |bool box(int x, int y) { true }
      """.stripMargin)
  }

  @Test
  def methodRedeclaration(): Unit = {
    expectSemanticException(
      """struct A {
        |   int box(int x, int y) { x }
        |   int box(int x, int y) { y }
        |}
      """.stripMargin)
    expectSemanticException(
      """struct A {
        |   int box(int x, int y) { x }
        |   bool box(int x, int y) { true }
        |}
      """.stripMargin)
  }

  @Test
  def functionOverload(): Unit = {
    val src =
      """int add(int x, int y) { x + y }
        |int add(int x, bool y) {
        |   int result = x;
        |   if (y) {
        |       result = x + 1;
        |   } else {};
        |   result
        |}
        |bool add(bool x, bool y) { x != y }
        |bool box() {
        |   int a = add(2, 5);
        |   int b = add(3, true);
        |   bool c = add(true, false);
        |   a == 7 && b == 4 && c
        |}
      """.stripMargin
    runTest(src, 1)
  }

  @Test
  def typeMismatch(): Unit = {
    expectSemanticException("int box() { true }")
    expectSemanticException("bool box() { 1 }")
    expectSemanticException("bool box() {  }")
    expectSemanticException(
      """bool box() {
        |    int a;
        |    a = 5;
        |    a
        |}
      """.stripMargin)
    expectSemanticException(
      """void box() {
        |    int a;
        |    a = true;
        |}
      """.stripMargin)
    expectSemanticException(
      """void box() {
        |    bool a;
        |    a = 42;
        |}
      """.stripMargin)
    expectSemanticException(
      """int dup(int x) { x * 2 }
        |bool box() {
        |   dup(2)
        |}
      """.stripMargin)
  }

  @Test
  def uninitializedVariable(): Unit = {
      val src =
        """int box() {
          |    int a, b;
          |    a = b + 2;
          |    a
          |}
      """.stripMargin
    runTest(src, 2)
  }

  @Test
  def structFields(): Unit = {
    val src =
      """struct Rectangle {
        |   int w, h;
        |}
        |int box() {
        |   Rectangle r;
        |   r = new Rectangle();
        |   r.w = 3;
        |   r.h = 4;
        |   r.w * r.h
        |}
      """.stripMargin
    runTest(src, 12)
  }

  @Test
  def structAccessViolation(): Unit = {
    val src =
      """struct Safe {
        |   private int secret;
        |}
        |void box() {
        |   Safe s;
        |   s = new Safe();
        |   s.secret = 1;
        |}
      """.stripMargin
    expectSemanticException(src)
  }

  @Test
  def uninitializedStructField(): Unit = {
    val src =
      """struct A {
        |   int a;
        |}
        |int box() {
        |   A a;
        |   a = new A();
        |   a.a
        |}
      """.stripMargin
    runTest(src, 0)
  }

  @Test
  def conditionalStructInit(): Unit = {
    val src =
      """struct A {
        |   int a;
        |}
        |int box() {
        |   A a;
        |   if (true) {
        |     a = new A();
        |   } else {};
        |   a.a
        |}
      """.stripMargin
    runTest(src, 0)
  }

  @Test
  def conditionalPrimitiveInit(): Unit = {
    val src =
      """int box() {
        |   int a, b = 0;
        |   if (true) {
        |       a = 1;
        |   } else {
        |       b = 2;
        |   };
        |   a + b
        |}
      """.stripMargin
    runTest(src, 1)
  }

  @Test
  def passStructToFunction(): Unit = {
    val src =
      """struct A { int a; }
        |int getA(A a) { a.a }
        |int box() {
        |   A a;
        |   a = new A();
        |   a.a = 5;
        |   getA(a)
        |}
      """.stripMargin
    runTest(src, 5)
  }

  @Test
  def returnStructFromFunction(): Unit = {
    val src =
      """struct A { int a; }
        |A constructA(int val) {
        |   A a = new A();
        |   a.a = val;
        |   a
        |}
        |bool box() {
        |   A a1 = constructA(5);
        |   A a2 = constructA(6);
        |   a1.a == 5 && a2.a == 6
        |}
      """.stripMargin
    runTest(src, 1)
  }

  @Test
  def structFieldInStruct(): Unit = {
    val src =
      """struct A { int x; }
        |struct B { A a; }
        |A constructA() {
        |   A a;
        |   a = new A();
        |   a.x = 10;
        |   a
        |}
        |B constructB() {
        |   B b;
        |   b = new B();
        |   b.a = constructA();
        |   b
        |}
        |int box() {
        |   B b;
        |   A a;
        |   b = constructB();
        |   a = b.a;
        |   a.x
        |}
      """.stripMargin
    runTest(src, 10)
  }

  @Test
  def chainingFieldReadAccess(): Unit = {
    val src =
      """struct A { int x; }
        |struct B { A a; }
        |A constructA() {
        |   A a;
        |   a = new A();
        |   a.x = 10;
        |   a
        |}
        |B constructB() {
        |   B b;
        |   b = new B();
        |   b.a = constructA();
        |   b
        |}
        |int box() {
        |   B b;
        |   b = constructB();
        |   b.a.x
        |}""".stripMargin
    runTest(src, 10)
  }

  @Test
  def chainingFieldWriteAccess(): Unit = {
    val src =
      """struct A { int x; }
        |struct B { A a; }
        |B constructB() {
        |   B b;
        |   b = new B();
        |   b.a = new A();
        |   b
        |}
        |int box() {
        |   B b;
        |   b = constructB();
        |   b.a.x = 10;
        |   b.a.x
        |}""".stripMargin
    runTest(src, 10)
  }

  @Test
  def fieldAccessAssoc(): Unit = {
    val src =
      """struct Rectangle {
        |   int w, h, s;
        |}
        |int box() {
        |   Rectangle r;
        |   r = new Rectangle();
        |   r.w = 10;
        |   r.h = 15;
        |   r.s = r.w * r.h;
        |   r.s
        |}
      """.stripMargin
    runTest(src, 150)
  }

  @Test
  def fieldDuplication(): Unit = {
    expectSemanticException(
      """struct A {
        |   int a;
        |   int a;
        |}
      """.stripMargin)
  }

  @Test
  def variablesInit(): Unit = {
    val src =
      """struct A { int a; }
        |int box() {
        |   A a = new A();
        |   int b = 5, c = 6;
        |   a.a = 4;
        |   a.a * b * c
        |}
      """.stripMargin
    runTest(src, 120)
  }

  @Test
  def structMethods(): Unit = {
    val src =
      """struct Rectangle {
        |   int w, h;
        |   int square() {
        |     this.w * this.h
        |   }
        |}
        |int box() {
        |   Rectangle r = new Rectangle();
        |   r.w = 4;
        |   r.h = 5;
        |   r.square()
        |}
      """.stripMargin
    runTest(src, 20)
  }

  @Test
  def privateStructMethods(): Unit = {
    expectSemanticException(
      """struct A {
        |   private int a;
        |   private int getA() { this.a }
        |   constructor(int a) { this.a = a; }
        |}
        |int box() {
        |   A a = new A(5);
        |   a.getA()
        |}
      """.stripMargin)
  }

  @Test
  def structMethodsWithParameters(): Unit = {
    val src =
      """struct Rectangle {
        |   int w, h;
        |   bool equals(Rectangle other) {
        |       this.w == other.w && this.h == other.h
        |   }
        |}
        |Rectangle constructRectangle(int w, int h) {
        |   Rectangle r = new Rectangle();
        |   r.w = w;
        |   r.h = h;
        |   r
        |}
        |bool box() {
        |   Rectangle r1 = constructRectangle(4, 5);
        |   Rectangle r2 = constructRectangle(4, 5);
        |   Rectangle r3 = constructRectangle(5, 6);
        |   r1.equals(r2) && (r1.equals(r3) == false)
        |}
      """.stripMargin
    runTest(src, 1)
  }

  @Test
  def structMethodsOverload(): Unit = {
    val src =
      """struct Rectangle {
        |   int w, h;
        |   bool equals(Rectangle other) {
        |     this.w == other.w && this.h == other.h
        |   }
        |   bool equals(int w, int h) {
        |     this.w == w && this.h == h
        |   }
        |}
        |Rectangle constructRectangle(int w, int h) {
        |   Rectangle r = new Rectangle();
        |   r.w = w;
        |   r.h = h;
        |   r
        |}
        |bool box() {
        |   Rectangle r1 = constructRectangle(1, 2);
        |   Rectangle r2 = constructRectangle(1, 2);
        |   Rectangle r3 = constructRectangle(3, 4);
        |   r1.equals(r2) && r1.equals(1, 2) &&
        |     (r1.equals(r3) == false) && (r1.equals(3, 4) == false)
        |}
      """.stripMargin
    runTest(src, 1)
  }

  @Test
  def constructorTest(): Unit = {
    val src =
      """struct Rectangle {
        |   private int w, h;
        |   constructor(int w, int h) {
        |     this.w = w;
        |     this.h = h;
        |   }
        |   int getS() { this.w * this.h }
        |}
        |int box() {
        |   new Rectangle(3, 4).getS()
        |}
      """.stripMargin
    runTest(src, 12)
  }

  @Test
  def defaultConstructor(): Unit = {
    val src =
      """struct A { int a; }
        |int box() {
        |   A a = new A();
        |   a.a
        |}
      """.stripMargin
    runTest(src, 0)
  }

  @Test
  def noDefaultConstructor(): Unit = {
    expectSemanticException(
      """struct A {
        |   int a;
        |   constructor(int a) { this.a = a; }
        |}
        |void box() {
        |   A a = new A();
        |}
      """.stripMargin)
  }

  @Test
  def defaultSuperConstructor(): Unit = {
    val src =
      """struct A { int a; }
        |struct B : A { int b; }
        |bool box() {
        |   B b = new B();
        |   b.a == 0 && b.b == 0
        |}
      """.stripMargin
    runTest(src, 1)
  }

  @Test
  def returningSubtype(): Unit = {
    val src =
      """struct A { int a; }
        |struct B : A {
        |   int b;
        |   constructor(int a, int b) {
        |     this.a = a;
        |     this.b = b;
        |   }
        |}
        |A makeA() { new B(23, 42) }
        |int box() {
        |   A a = makeA();
        |   a.a
        |}
      """.stripMargin
    runTest(src, 23)
  }

  @Test
  def assigningToSubtype(): Unit = {
    val src =
      """struct A {
        |   int a;
        |}
        |struct B : A {
        |   constructor(int a) { this.a = a; }
        |}
        |int box() {
        |   A a;
        |   a = new B(4);
        |   a.a
        |}
        """.stripMargin
    runTest(src, 4)
  }

  @Test
  def initWithSubtype(): Unit = {
    val src =
      """struct A {
        |   int a;
        |}
        |struct B : A {
        |   constructor(int a) { this.a = a; }
        |}
        |int box() {
        |   A a = new B(4);
        |   a.a
        |}
      """.stripMargin
    runTest(src, 4)
  }

  @Test
  def functionsContravariantByArguments(): Unit = {
    val src =
      """struct A { int a; }
        |struct B : A {
        |   int b;
        |   constructor(int a, int b) {
        |       this.a = a;
        |       this.b = b;
        |   }
        |}
        |int getA(A a) {
        |   a.a
        |}
        |int box() {
        |   B b = new B(23, 42);
        |   getA(b)
        |}
      """.stripMargin
    runTest(src, 23)
  }

  @Test
  def inheritingMethods(): Unit = {
    val src =
      """struct A {
        |   private int a;
        |   int getA() { this.a }
        |}
        |struct B : A {
        |   constructor(int a) { this.a = a; }
        |}
        |int box() {
        |   B b = new B(5);
        |   b.getA()
        |}
      """.stripMargin
    runTest(src, 5)
  }

  @Test
  def inheritingPrivateMethods(): Unit = {
    val src =
      """struct A {
        |   private int a;
        |   private int getA() { this.a }
        |}
        |struct B : A {
        |   constructor(int a) { this.a = a; }
        |   int get() { this.getA() }
        |}
        |int box() {
        |   B b = new B(5);
        |   b.get()
        |}
      """.stripMargin
    runTest(src, 5)
  }

  @Test
  def inheritingFields(): Unit = {
    val src =
      """struct A {
        |   int a;
        |   int b;
        |}
        |struct B : A {
        |   constructor(int a) { this.a = a; }
        |}
        |bool box() {
        |   B b = new B(5);
        |   b.b = 2;
        |   b.a == 5 && b.b == 2
        |}
      """.stripMargin
    runTest(src, 1)
  }

  @Test
  def simpleTailCall(): Unit = {
    val src =
      """int f(int x, int acc) {
        |   if (x == 0) {
        |     acc
        |   } else {
        |     f(x - 1, acc + 2)
        |   }
        |}
        |int box() {
        |   f(1000000, 0)
        |}
      """.stripMargin
    runTest(src, 2000000)
  }

  @Test
  def infiniteRecursion(): Unit = {
    expectSemanticException(
      """int box() {
        |   box()
        |}
      """.stripMargin)
    expectSemanticException(
      """int box() {
        |   if (true) {
        |     box()
        |   } else {
        |     box()
        |   }
        |}
      """.stripMargin)
  }

  @Test
  def simpleMethodTailCall(): Unit = {
    val src =
      """struct Num {
        |   private int n;
        |   constructor(int n) { this.n = n }
        |   private int f(int n, int acc) {
        |     if (n == 0) {
        |       acc
        |     } else {
        |       this.f(n - 1, acc + 2)
        |     }
        |   }
        |   int f() { this.f(this.n, 0) }
        |}
        |int box() {
        |   Num num = new Num(1000000);
        |   num.f()
        |}
      """.stripMargin
    runTest(src, 2000000)
  }

  @Test
  def accumulatedFunctionTailCall(): Unit = {
    val src =
      """int f(int x) {
        |   if (x == 0) {
        |     5
        |   } else {
        |     f(x - 1) + 2
        |   }
        |}
        |int box() {
        |   f(1000000)
        |}
      """.stripMargin
    runTest(src, 2000005)
  }

  @Test
  def accumulatedMethodTailCall(): Unit = {
    val src =
      """struct Num {
        |   private int n;
        |   constructor(int n) { this.n = n }
        |   private int f(int n) {
        |     if (n == 0) {
        |       5
        |     } else {
        |       this.f(n - 1) + 2
        |     }
        |   }
        |   int f() { this.f(this.n) }
        |}
        |int box() {
        |   Num num = new Num(1000000);
        |   num.f()
        |}
      """.stripMargin
    runTest(src, 2000005)
  }

  @Test
  def recursiveFibonacci(): Unit = {
    val src =
      """int fib(int n) {
        |   if (n == 0 || n == 1) {
        |     1
        |   } else {
        |     fib(n - 1) + fib(n - 2)
        |   }
        |}
        |int box() {
        |   fib(10)
        |}
      """.stripMargin
    runTest(src, 89)
  }
}
