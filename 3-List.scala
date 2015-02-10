sealed trait MyList[+A]
object MyNil extends MyList[Nothing] {
    override def toString = "Nil"
}

case class Cons[+A](head: A, tail: MyList[A]) extends MyList[A]

object MyList {
    def apply[A](as: A*): MyList[A] = 
        if (as.isEmpty) MyNil
        else Cons(as.head, apply(as.tail: _*))
}

def tail[A](xs: MyList[A]): MyList[A] = xs match {
    case MyNil => throw new Exception("Can't tail an empty list")
    case Cons(_, xs1) => xs1
}

def init[A](xs: MyList[A]): MyList[A] = xs match {
    case MyNil => throw new Exception("Can't init an empty list")
    case Cons(a, MyNil) => MyNil 
    case Cons(a, xs1) => Cons(a, init(xs1))
}

def drop[A](xs: MyList[A], n: Int): MyList[A] = xs match {
    case MyNil => MyNil
    case Cons(x, xs1) => if (n == 0) xs else drop(xs1, n - 1)
}

def dropWhile[A](xs: MyList[A], f: A => Boolean): MyList[A] = xs match {
    case MyNil => MyNil
    case Cons(x, xs1) => if (f(x)) dropWhile(xs1, f) else xs
}

def setHead[A](x: A, xs: MyList[A]) = xs match {
    case MyNil => throw new Exception("No head to replace")
    case Cons(x1, xs) => Cons(x, xs)
}

def foldRight[A,B](ys: MyList[A], z: B)(f: (A,B) => B):B = ys match {
    case MyNil => z 
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
}

def length[A](xs: MyList[A]): Int = foldRight(xs, 0)((a:A, c:Int) => 1 + c)

// Exercise 3.10
def foldLeft[A,B](ys: MyList[A], z: B)(f: (B, A) => B):B = ys match {
    case MyNil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
}

def reverse[A](xs: MyList[A]): MyList[A] = foldLeft(xs, MyNil:MyList[A])((ys: MyList[A], a:A) => Cons(a, ys))

def append[A](xsA: MyList[A], xsB: MyList[A]): MyList[A] = foldRight(xsA, xsB)((x: A, xs: MyList[A]) => Cons(x, xs))

def concatenate[A](xs: MyList[MyList[A]]): MyList[A] = foldRight(xs, MyNil:MyList[A])((xs1: MyList[A], accum: MyList[A]) => append(xs1, accum))

def addOne(xs: MyList[Int]): MyList[Int] = foldRight(xs, MyNil:MyList[Int])((x:Int, accum:MyList[Int]) => Cons(x + 1, accum))

def map[A,B](xs: MyList[A])(f: A => B): MyList[B] = foldRight(xs, MyNil:MyList[B])((a: A, accum:MyList[B]) => Cons(f(a), accum))

def filter[A](xs: MyList[A])(f: A => Boolean): MyList[A] = foldRight(xs, MyNil:MyList[A])((a: A, accum: MyList[A]) => if (f(a)) Cons(a, accum) else accum )

def flatMap[A, B](xs: MyList[A])(f: A => MyList[B]): MyList[B] = foldRight(xs, MyNil:MyList[B])((a:A, accum:MyList[B]) => append(f(a), accum))

def filterThruFlatMap[A](xs: MyList[A])(f: A => Boolean): MyList[A] = flatMap(xs)(a => if (f(a)) MyList(a) else MyNil:MyList[A])

def addCorresponding(xs: MyList[Int], ys: MyList[Int]): MyList[Int] = (xs, ys) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (Cons(x, xs1), Cons(y, ys1)) => Cons((x + y), addCorresponding(xs1, ys1))
}

def zipWith[A, B, C](xs: MyList[A], ys: MyList[B])(f: (A, B) => C): MyList[C] = (xs, ys) match {
    case (MyNil, _) => MyNil
    case (_, MyNil) => MyNil
    case (Cons(x, xs1), Cons(y, ys1)) => Cons(f(x,y), zipWith(xs1, ys1)(f))
}

def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => return sub == Nil
    case ys@(x::xs) => ys.startsWith(sub) || hasSubSequence(xs, sub)
}
