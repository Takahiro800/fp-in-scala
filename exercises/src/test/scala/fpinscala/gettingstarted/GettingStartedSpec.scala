package fpinscala.gettingstarted

import wvlet.airspec.*

class GettingStartedSpec extends AirSpec {
  test("abs") {
    MyModule.abs(1) shouldBe 1
    MyModule.abs(0) shouldBe 0
    MyModule.abs(-1) shouldBe 1
  }

  test("fib") {
    MyModule.fib(1) shouldBe 0
    MyModule.fib(2) shouldBe 1
    MyModule.fib(3) shouldBe 1
    MyModule.fib(5) shouldBe 3
  }

  test("isSorted") {
    // Array[Int]
    PolymorphicFunctions.isSorted(
      Array[Int](),
      (x: Int, y: Int) => x < y
    ) shouldBe true
    PolymorphicFunctions.isSorted(
      Array(1, 2, 3),
      (x: Int, y: Int) => x < y
    ) shouldBe true

    // Array[String]
    PolymorphicFunctions.isSorted(
      Array[String](),
      (x: String, y: String) => x < y
    ) shouldBe true
    PolymorphicFunctions.isSorted(
      Array("a", "ab", "ac"),
      (x: String, y: String) => x < y
    ) shouldBe true
  }
}
