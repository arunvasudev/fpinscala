package myStream 

import Stream._

trait Stream[+A] {
    
    def isEmpty: Boolean

    def headOption: Option[A] = this match {
        case Cons(h, t) => Some(h())
        case Empty => None
    }

    def tailOption: Option[Stream[A]] = this match {
        case Cons(h, t) => Some(t())
        case Empty => None
    }

    def toList: List[A] = this match {
        case Cons(h, t) => h() :: t().toList
        case Empty => Nil
    }

    def take(n: Int): Stream[A] = this match {
        case Cons(h, t) => if (n == 0) (empty:Stream[A]) else cons(h(), t().take(n - 1))
        case Empty => empty 
    }

    def drop(n: Int): Stream[A] = this match {
        case Cons(h, t) => if (n == 0) this else t().drop(n - 1)
        case Empty => empty 
    }

    def takeWhile(f: A => Boolean): Stream[A] = this match {
        case Cons(h, t) if f(h()) => cons(h(), t().takeWhile(f))
        case Empty => empty 
    }

    def foldRight[B](z : => B)(f : (A, => B) => B): B = this match {
        case Cons(h, t) => f(h(), t().foldRight(z)(f))
        case _ => z
    }

    def foldLeft[B](z: => B)(f : (=> B, A) => B): B = this match {
        case Cons(h, t) => t().foldLeft(f(z, h()))(f)
        case _ => z
    }

    def foldLeftImp[B](z: => B)(f : (=> B, A) => B): B = {
        var curr = this
        var accum = z
        while(!curr.isEmpty){
            curr match {
                case Cons(h, t) => { 
                    accum = f(accum, h())
                    curr = t()
                }
                case _ => assert(false)
            }
        }

        accum
    }

    def exists(f: A => Boolean): Boolean = foldRight(false)((a, accum) => f(a) || accum)

    def forAll(f: A => Boolean): Boolean = foldRight(true)((a, accum) => f(a) && accum )

    def forAllRecursive(f: A => Boolean): Boolean = this match {
        case Empty => true
        case Cons(h, t) => return f(h()) && t().forAllRecursive(f)
    }

    def forAllImp(f: A => Boolean): Boolean = foldLeftImp(true)((accum, a) => accum && f(a))

    def takeWhileFR(f: A => Boolean): Stream[A] = 
        foldRight(empty[A])((a, accum) => if (f(a)) cons(a, accum) else Empty )

    def map[B](f: A => B): Stream[B] = foldRight(empty[B])((a, accum) => cons(f(a), accum))

    def filter(f: A => Boolean): Stream[A] = foldRight(empty[A])((a, accum) => if (f(a)) cons(a, accum) else accum)

    def append[B >: A](str: => Stream[B]): Stream[B] = foldRight(str)((a, accum) => cons(a, accum))

    def flatMap[B](f: A => Stream[B]): Stream[B] = foldRight(empty[B])((a, accum) => f(a).append(accum))

    def mapThruUnfold[B](f: A => B): Stream[B] = {
        def mapHelper(str: Stream[A]): Option[(B, Stream[A])] = str match {
            case Cons(h, t) => Some((f(h()), t()))
            case Empty => None
        }

        unfold(this)(mapHelper)
    }

    def takeThruUnfold(n: Int): Stream[A] = {
        def takeHelper(state: (Int, Stream[A])): Option[(A, (Int, Stream[A]))] = state match { case (n, str) =>
            if (n <= 0) None
            else {
                str match {
                    case Cons(h, t) => Some((h(), (n - 1, t())))
                    case Empty => None
                }
            }
        }

        unfold((n, this))(takeHelper)
    }

    def takeWhileThruUnfold(f: A => Boolean): Stream[A] = {
        def takeWhileHelper(str: Stream[A]): Option[(A, Stream[A])] = str match {
            case Cons(h, t) if f(h()) => Option((h(), t()))
            case _ => None
        }

        unfold(this)(takeWhileHelper)
    }

    def zipWithThruUnfold[B, C](strB: Stream[B])(f: (A, B) => C): Stream[C] = {
        def zipWithHelper(p: (Stream[A], Stream[B])): Option[(C, (Stream[A], Stream[B]))] = p match {
            case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
            case _ => None
        }

        unfold((this, strB))(zipWithHelper)
    }

    def zipAllThruUnfold[B](strB: Stream[B]): Stream[(A, B)] = {
        def zipAllHelper(p: (Stream[A], Stream[B])): Option[((A, B), (Stream[A], Stream[B]))] = p match {
            case (Cons(h1, t1), Cons(h2, t2)) => Some(((h1(), h2()), (t1(), t2())))
            case _ => None
        }

        unfold((this, strB))(zipAllHelper)
    }
}

case object Empty extends Stream[Nothing] {
    override def isEmpty = true
}

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] {
    override def toString: String = s"Cons(${h()}, ?)"
    override def isEmpty = false
}

object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
        lazy val head = hd
        lazy val tail = tl
        Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] = {
        if (as.isEmpty) empty else cons(as.head, apply(as.tail:_*))
    }

    def constant[A](a: A): Stream[A] = {
        lazy val cs:Stream[A] = cons(a, cs) 
        return cs
    }

    def from(n: Int): Stream[Int] = cons(n, from(n + 1))

    def fibs:Stream[Int] = {
        def fibsHelper(a: Int, b: Int): Stream[Int] = cons(a, fibsHelper(b, a+b))
        fibsHelper(1,1)
    }

    def unfold[A, S](z: S)(f: S => Option[(A, S)]) : Stream[A] = {
        val next = f(z)
        next match {
            case None => empty[A]
            case Some((a, s)) => cons(a, unfold(s)(f))
        }
    }

    def constant2[A](a: A): Stream[A] = {
        def nextConst[A](a: A): Option[(A, A)] = Some((a, a))
        unfold(a)(nextConst)
    }

    def from2(n: Int): Stream[Int] = {
        def nextInt(n1: Int): Option[(Int, Int)] = Some((n1, n1+1))
        unfold(n)(nextInt)
    }

    def fibs2: Stream[Int] = {
        def nextFib(p: (Int, Int)): Option[(Int, (Int, Int))] = p match {
            case (a, b) => Some((a, (b, a+b)))
        }

        unfold((1,1))(nextFib)
    }

    def ones2: Stream[Int] = constant2(1)
}
