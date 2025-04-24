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

  test("drop") {
    List.drop(Nil, 1) shouldBe Nil

    val l_1to5 = List(1, 2, 3, 4, 5)
    List.drop(l_1to5, 0) shouldBe l_1to5
    List.drop(l_1to5, 2) shouldBe List(3, 4, 5)
    List.drop(l_1to5, 5) shouldBe Nil
  }

  test("dropWhile") {
    List.dropWhile(Nil, (x: Int) => x < 0) shouldBe Nil

    val l_1to5 = List(1, 2, 3, 4, 5)
    List.dropWhile(l_1to5, (x: Int) => x > 0) shouldBe Nil
    List.dropWhile(l_1to5, (x: Int) => x < 2) shouldBe List(2, 3, 4, 5)
    List.dropWhile(l_1to5, (x: Int) => x % 2 == 0) shouldBe l_1to5
  }

  test("init") {
    val l_1to5 = List(1, 2, 3, 4, 5)
    List.init(l_1to5) shouldBe List(1, 2, 3, 4)
  }

  test("length") {
    test("should return 0 for an empty list") {
      List.length(Nil) shouldBe 0
    }

    test("should return the correct length for a non-empty list") {
      List.length(List(1, 2, 3)) shouldBe 3
      List.length(List("a", "b", "c", "d")) shouldBe 4
    }

    test("should handle a single-element list") {
      List.length(List(42)) shouldBe 1
    }
  }
}
