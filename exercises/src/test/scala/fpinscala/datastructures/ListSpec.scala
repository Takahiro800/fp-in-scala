package fpinscala.datastructures

import wvlet.airspec.*

class DataStructuresSpec extends AirSpec {
  test("tail") {
    List.tail(Nil) shouldBe Nil
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)

    List.tail(List("hoge", "fuga")) shouldBe List("fuga")
  }
}
