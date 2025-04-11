package fpinscala.gettingstarted

import wvlet.airspec.*

class GettingStartedSpec extends AirSpec {
  test("abs") {
    MyModule.abs(1) shouldBe 1
    MyModule.abs(0) shouldBe 0
    MyModule.abs(-1) shouldBe 1
  }
}
