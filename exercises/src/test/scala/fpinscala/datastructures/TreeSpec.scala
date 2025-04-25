package fpinscala.datastructures

import wvlet.airspec.*

class TreeSpec extends AirSpec {
  val leaf = Leaf(42)
  val branch = Branch(Leaf(1), Leaf(2))
  val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

  test("size") {
    test("when only single leaf") {
      Tree.size(leaf) shouldBe 1
    }

    test("when 1 branch and 2 leaves") {
      Tree.size(branch) shouldBe 3
    }

    test("when 3 depth") {
      Tree.size(tree) shouldBe 5
    }
  }

  test("maximum") {
    test("when only single leaf") {
      Tree.maximum(leaf) shouldBe leaf.value
    }

    test("when 1 branch and 2 leaves") {
      Tree.maximum(branch) shouldBe 2
    }

    test("when 3 depth") {
      Tree.maximum(tree) shouldBe 3
    }
  }
}
