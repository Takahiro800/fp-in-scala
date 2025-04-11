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
}
