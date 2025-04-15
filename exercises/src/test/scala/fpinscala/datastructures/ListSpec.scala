package fpinscala.datastructures

import wvlet.airspec.*

class DataStructuresSpec extends AirSpec {
  test("tail") {
    List.tail(Nil) shouldBe Nil
    List.tail(List(1, 2, 3)) shouldBe List(2, 3)

    List.tail(List("hoge", "fuga")) shouldBe List("fuga")
  }

  test("setHead") {
    List.setHead(Nil, 1) shouldBe List(1)
    List.setHead(List(2, 3), 1) shouldBe List(1, 3)
    List.setHead(List("hoge", "fuga"), "piyo") shouldBe List("piyo", "fuga")
  }
}
