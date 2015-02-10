sealed trait Tree[+A]{
    def size: Int
    def depth: Int
    def map[B](f: A => B): Tree[B]
    def fold[B](b: B)(f: (A,B) => B): B
}

case class Leaf[A](value: A) extends Tree[A] {
    override def size = 1
    override def depth = 0
    override def map[B](f: A => B) = Leaf(f(value))
    override def fold[B](b: B)(f: (A, B) => B) = f(value, b)
}

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A] {
    override def size = 1 + left.size + right.size
    override def depth = 1 + left.depth.max(right.depth)
    override def map[B](f: A => B) = Branch(left.map(f), right.map(f))
    override def fold[B](b: B)(f: (A, B) => B): B = right.fold(left.fold(b)(f))(f)
}

object Tree {
    def maximum(t: Tree[Int]): Int = t match {
        case Leaf(value) => return value
        case Branch(left, right) => maximum(left).max(maximum(right))
    }
}
