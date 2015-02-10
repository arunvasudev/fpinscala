package myOption

sealed abstract class Option[+A] {
    def map[B](f: A => B): Option[B] = if (isEmpty) None else Some(f(get))
    def flatMap[B](f: A => Option[B]): Option[B] = if (isEmpty) None else f(get)
    def getOrElse[B >: A](default: => B): B = if (isEmpty) default else get
    def orElse[B >: A](ob: => Option[B]): Option[B] = if (isEmpty) ob else this
    def filter(f: A => Boolean): Option[A] = if (isEmpty || f(get)) this else None
    def get: A = throw new Exception("Not implemented")
    def isEmpty: Boolean 
}

case object None extends Option[Nothing] {
    def isEmpty = true
}

case class Some[+A](x: A) extends Option[A] {
    def isEmpty = false
    override def get = x
}

object Try {
    def apply[A](f: => A): Option[A] = try {Some(f)} catch { case _:Exception => None }
}
