package myEither

abstract class Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] = this match {
        case Left(e) => Left(e) 
        case Right(a) => Right(f(a))
    }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => Left(e) 
        case Right(a) => f(a)
    }

    def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
        case Left(e) => b
        case Right(a) => Right(a)
    }

    def map2[EE >: E, B, C](b: => Either[EE, B])(f: (A, B) => C): Either[EE, C] = this match {
        case Left(e) => Left(e)
        case Right(a) => b match {
            case Left(e) => Left(e)
            case Right(b) => Right(f(a, b))
        }
    }
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object TryE {
    def apply[E, A](a: => A, e: => E): Either[E, A] = try {
        Right(a)
    } catch {
        case _:Exception => Left(e)
    }
}
