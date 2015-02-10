import myEither._

def sequence[E, A](xs: List[Either[E, A]]): Either[E, List[A]] = xs match {
    case Nil => Right(Nil:List[A])
    case (x::xs1) => x.map2(sequence(xs1))((y:A, ys:List[A]) => y::ys)
}

def strToInt(str: String): Either[String, Int] = TryE(str.toInt, s"Failed to parse ${str} to integer")

def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = { 
    println("traverse(" + as + ")")
    as match {
        case Nil => Right(Nil:List[B])
        case (x::xs1) => f(x).map2(traverse(xs1)(f))((y:B, ys:List[B]) => y::ys)
    }
}

