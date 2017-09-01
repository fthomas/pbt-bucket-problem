
abstract class Tree[A, +B] {
  def sizeNodes: Int
  def sizeLeaf: Int
  def size: Int = sizeNodes + sizeLeaf
}

case class Node[A, B](l: Tree[A, B], r: Tree[A, B], b: B) extends Tree[A, B] {
  override def sizeNodes: Int = 1 + l.sizeNodes + r.sizeNodes
  override def sizeLeaf: Int = l.sizeLeaf + r.sizeLeaf
}

case class Leaf[A](a: A) extends Tree[A, Nothing] {
  override def sizeNodes: Int = 0
  override def sizeLeaf: Int = 1
}

// R = Tree[A, B]
// def fold[A, B, R](tree: Tree[A, B])
// (ifLeaf: A => Tree[A, B])
// (ifNode: (Tree[A, B], Tree[A, B], B) => Tree[A, B])
// : Tree[A, B] =


def depth[A, B](tree: Tree[A, B]): Int =
  fold(tree)(_ => 1)((r1, r2, b) => (r1 max r2) + 1)
/*
  tree match {
    case Leaf(_) => 1
    case Node(l, r,_) => (l.depth max r.depth) + 1
  }
*/

def map[A, B, C](tree: Tree[A, B])(f: A => C): Tree[C, B] =
  fold(tree)(a => Leaf(f(a)): Tree[C, B])((r1, r2, b) => Node(r1, r2, b))
/*
  tree match {
    case Leaf(a) => Leaf(f(a))
    case Node(l, r, b) => Node(map(l)(f), map(r)(f), b)
  }*/

def maximum(tree: Tree[Int, _]): Int =
  fold(tree)(a => a)((r1, r2, b) => r1 max r2)

/*tree match {
    case Leaf(a) => a
    case Node(l, r, _) =>
      maximum(l) max maximum(r)
  }*/

def fold[A, B, R](tree: Tree[A, B])(ifLeaf: A => R)(ifNode: (R, R, B) => R): R =
  tree match {
    case Leaf(a) => ifLeaf(a)
    case Node(l, r, b) =>
      ifNode(fold(l)(ifLeaf)(ifNode), fold(r)(ifLeaf)(ifNode), b)
  }


map(Node(Node(Leaf(100), Leaf(20), "Test"), Leaf(300), "Hallo"))(_ * 300)

