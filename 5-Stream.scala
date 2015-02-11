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
}
