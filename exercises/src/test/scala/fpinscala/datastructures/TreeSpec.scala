package fpinscala.datastructures

import wvlet.airspec.*

class TreeSpec extends AirSpec {
  test("size") {
    test("when only single leaf") {
      val leaf = Leaf(42)
      assert(Tree.size(leaf) == 1)
    }

    test("when 1 branch and 2 leaves") {
      val branch = Branch(Leaf(1), Leaf(2))
      assert(Tree.size(branch) == 3)
    }

    test("when 3 depth") {
      val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))
      assert(Tree.size(tree) == 5)
    }
  }
}
