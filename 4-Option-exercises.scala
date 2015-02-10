import myOption._

def variance(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else {
        val len = xs.length
        val m = xs.foldLeft(0.0)(_ + _) / len
        val v = xs.foldLeft(0.0)((accum, x) => math.pow(x - m, 2) + accum ) / len
        Some(v)
    }

def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

def map2[A, B, C](a: => Option[A], b: => Option[B])(f: (A, B) => C): Option[C] = 
    if (a.isEmpty || b.isEmpty) None else Some(f(a.get, b.get))

def sequence[A](xs: List[Option[A]]): Option[List[A]] = xs match {
    case Nil => Some(Nil:List[A])
    case (x::xs1) => map2(x, sequence(xs1))((y:A, ys:List[A]) => y::ys)
}

def strToInt(str: String): Option[Int] = Try(str.toInt)

def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = { 
    println("traverse(" + as + ")")
    as match {
        case Nil => Some(Nil:List[B])
        case (x::xs1) => map2(f(x), traverse(xs1)(f))((y:B, ys:List[B]) => y::ys)
    }
}
