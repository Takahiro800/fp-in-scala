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

  test("concat") {
    val l1 = List(1, 2, 3)
    val l2 = List(4, 5, 6)
    val l3 = List(7, 8, 9)

    List.concat(List(l1, l2, l3)) shouldBe List(1, 2, 3, 4, 5, 6, 7, 8, 9)
  }

  test("add1") {
    val l1 = List(1, 2, 3)
    List.add1(l1) shouldBe List(2, 3, 4)
  }

  test("doubleToString") {
    List.doubleToString(List(1.0, 2.1, 3)) shouldBe List("1.0", "2.1", "3.0")
  }

  test("map") {
    val doubles = List(1.0, 2.1, 3)
    val strings = List("1.0", "2.1", "3.0")

    List.map(doubles)(_.toString) shouldBe strings
    List.doubleToString(doubles) shouldBe strings
  }

  test("filter") {
    val nums = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val odds = List(1, 3, 5, 7, 9)

    test("original") {
      List.filter(nums)(_ % 2 != 0) shouldBe odds
    }

    test("version flatMap") {
      List.filterByFlatMap(nums)(_ % 2 != 0) shouldBe odds
    }
  }

  test("faltMap") {
    val nums = List(1, 2, 3)
    val expects = List(1, 1, 2, 2, 3, 3)

    test("original") {
      List.flatMap(nums)(i => List(i, i)) shouldBe expects
    }

    test("version2") {
      List.flatMap2(nums)(i => List(i, i)) shouldBe expects
    }
  }

  test("addPairwise") {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6)

    test("when length is same") {
      List.addPairwise(a, b) shouldBe List(5, 7, 9)
    }

    test("a is shorter than b") {
      List.addPairwise(List(1), a) shouldBe List(2)
    }

    test("a.length is smaller than b.length") {
      List.addPairwise(List(1), b) shouldBe List(5)
    }
  }

  test("zipWith") {
    val a = List(1, 2, 3)
    val b = List(4, 5, 6)

    test("when length is same") {
      List.zipWith(a, b)(_ + _) shouldBe List(5, 7, 9)
    }

    test("a is shorter than b") {
      List.zipWith(List(1), a)(_ + _) shouldBe List(2)
    }

    test("a.length is smaller than b.length") {
      List.zipWith(List(1), b)(_ + _) shouldBe List(5)
    }
  }

  test("hasSubsequence") {
    val sup = List(1, 2, 3, 4, 5, 6)

    test("sub is Nil") {
      val sub = Nil: List[Int]
      List.hasSubsequence(sup, sub) shouldBe true
    }

    test("sub List(1,2,3)") {
      val sub = List(1, 2, 3)
      List.hasSubsequence(sup, sub) shouldBe true
    }

    test("sub List(1,3)") {
      val sub = List(1, 3)
      List.hasSubsequence(sup, sub) shouldBe false
    }

    test("sub List(10)") {
      val sub = List(10)
      List.hasSubsequence(sup, sub) shouldBe false
    }
  }
}
