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

  test("foldLeft") {
    test("should return the initial value for an empty list") {
      List.foldLeft(Nil: List[Int], 0)(_ + _) shouldBe 0
    }

    test("should correctly sum elements of a list") {
      List.foldLeft(List(1, 2, 3, 4), 0)(_ + _) shouldBe 10
    }

    test("should correctly multiply elements of a list") {
      List.foldLeft(List(1, 2, 3, 4), 1)(_ * _) shouldBe 24
    }

    test("should handle string concatenation") {
      List.foldLeft(List("a", "b", "c"), "")(_ + _) shouldBe "abc"
    }

    test("should be tail-recursive for large lists") {
      val largeList = List.fromScalaList(scala.List.fill(100000)(1))
      List.foldLeft(largeList, 0)(_ + _) shouldBe 100000
    }

    test("sumByFoldLeft") {
      test("empty List") {
        List.lengthByFoldLeft(Nil) shouldBe 0
      }

      test("simple List") {
        List.sumByFoldLeft(List(1, 2, 3, 4)) shouldBe 10
      }

      test("large List") {
        val largeList = List.fromScalaList(scala.List.fill(1000000)(1))
        List.sumByFoldLeft(largeList) shouldBe 1000000
      }
    }

    test("productByFoldLeft") {
      test("empty List") {
        List.productByFoldLeft(Nil) shouldBe 1
      }

      test("simple List") {
        List.productByFoldLeft(List(1, 2, 3, 4)) shouldBe 24
      }

      test("large List") {
        val largeList = List.fromScalaList(scala.List.fill(1000000)(1))
        List.productByFoldLeft(largeList) shouldBe 1
      }
    }

    test("lengthByFoldLeft") {
      test("empty List") {
        List.lengthByFoldLeft(Nil) shouldBe 0
      }
      test("simple List") {
        List.lengthByFoldLeft(List(1, 2, 3, 4)) shouldBe 4
      }

      test("large List") {
        val largeList = List.fromScalaList(scala.List.fill(1000000)(1))
        List.lengthByFoldLeft(largeList) shouldBe 1000000
      }
    }
  }

  test("reverse") {
    test("when empty") {
      List.reverse(Nil) shouldBe Nil
    }

    test("when simple List") {
      List.reverse(List(1, 2, 3)) shouldBe List(3, 2, 1)
    }
  }

  test("appendByFoldRight") {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    val nil: List[Int] = Nil

    test("when l1 is empty") {
      List.appendByFoldRight(nil, l2) shouldBe l2
    }

    test("when l2 is empty") {
      List.appendByFoldRight(l1, nil) shouldBe l1
    }

    test("when simple") {
      List.appendByFoldRight(l1, l2) shouldBe List.append(l1, l2)
    }
  }
}
