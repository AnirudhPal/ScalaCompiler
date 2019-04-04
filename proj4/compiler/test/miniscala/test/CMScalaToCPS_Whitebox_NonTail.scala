package miniscala.test

import ok.AllOKTests

import miniscala.test.infrastructure.CPSHighTest
import org.junit.Test

/** Whitebox testing for entire program outputs */
class CMScalaToCPS_Whitebox_NonTail extends CPSHighTest {

  // TODO: Test recursive functions

  @Test def testNonTailLiteral = {
    testCPSHighTreeEquality("3", "vall v$1 = 3; vall v$2 = 0; halt(v$2)")
  } 

  @Test def testNonTailMultiLet =
    testCPSHighTreeEquality("val x = 1; val y = 2; y",
        "vall v$1 = 1; valp v$2 = id(v$1); vall v$3 = 2; valp v$4 = id(v$3); vall v$5 = 0; halt(v$5)")


  // TODO add more tests
	@Test def testNonTailMultiVar =
		testCPSHighTreeEquality("var x = 10; x = 20; x", "vall v$1 = 1; valp v$2 = block-alloc-242(v$1); vall v$3 = 0; vall v$4 = 10; valp v$5 = block-set(v$2, v$3, v$4); vall v$6 = 0; vall v$7 = 20; valp v$8 = block-set(v$2, v$6, v$7); valp v$9 = id(v$7); vall v$10 = 0; valp v$11 = block-get(v$2, v$10); vall v$12 = 0; halt(v$12)")

	@Test def testNonTailFunc =
		testCPSHighTreeEquality("def foo(bar: Int) = bar + 3; foo(3)", "deff v$1(v$2, v$3) = { vall v$4 = 3; valp v$5 = v$3 + v$4; v$2(v$5) }; vall v$6 = 3; defc v$7(v$8) = { vall v$9 = 0; halt(v$9) }; v$1(v$7, v$6)")

	@Test def testNonTailIf =
		testCPSHighTreeEquality("val x = 2; if(x == 2) 3 else 4", "vall v$1 = 2; valp v$2 = id(v$1); defc v$3(v$4) = { vall v$5 = 0; halt(v$5) }; defc v$6() = { vall v$7 = 3; v$3(v$7) }; defc v$8() = { vall v$9 = 4; v$3(v$9) }; vall v$10 = 2; if (v$2 == v$10) v$6 else v$8")

	@Test def testNonTailLoop =
		testCPSHighTreeEquality("var x = 5; while(x == 0) x = 0; x", "vall v$1 = 1; valp v$2 = block-alloc-242(v$1); vall v$3 = 0; vall v$4 = 5; valp v$5 = block-set(v$2, v$3, v$4); defc v$6(v$7) = {defc v$8() = {vall v$9 = 0; valp v$10 = block-get(v$2, v$9); vall v$11 = 0; halt(v$11)}; defc v$12() = {vall v$13 = 0; vall v$14 = 0; valp v$15 = block-set(v$2, v$13, v$14); valp v$16 = id(v$14); vall v$17 = (); v$6(v$17)}; vall v$18 = 0; valp v$19 = block-get(v$2, v$18); vall v$20 = 0; if (v$19 == v$20) v$12 else v$8}; vall v$21 = ();v$6(v$21)")
}
