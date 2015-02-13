package randGen

trait RNG {
    def nextInt: (Int, RNG)
}

object RNG {

    type Rand[+A] = RNG => (A, RNG)

    def map[A, B](s: Rand[A])(f: A => B): Rand[B] = 
        rng => {
            val (a, rng2) = s(rng)
            (f(a), rng2)
        }

    def unit[A](a: A): Rand[A] = rng => (a, rng)

    def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng0 => {
        val (a, rng1) = ra(rng0)
        val (b, rng2) = rb(rng1)
        (f(a, b), rng2)
    }

    def _int(rng0: RNG): (Int, RNG) = rng0.nextInt

    def _nonNegativeInt(rng0: RNG): (Int, RNG) = {
        val (n, rng1) = int(rng0)
        if (n == Int.MinValue) (Int.MaxValue, rng1) else (n.abs, rng1)
    }

    def _double(rng0: RNG): (Double, RNG) = {
        val (n, rng) = nonNegativeInt(rng0)
        val d = (Int.MaxValue.toDouble - n.toDouble)/Int.MaxValue.toDouble;
        return (d, rng)
    }

    def _intDouble(rng0: RNG): ((Int, Double), RNG) = {
        val (n, rng1) = _int(rng0)
        val (d, rng2) = _double(rng1)
        ((n, d), rng2)
    }
    
    def _doubleInt(rng0: RNG): ((Double, Int), RNG) = {
        val ((n, d), rng1) = _intDouble(rng0)
        ((d, n), rng0)
    }

    def _double3(rng0: RNG): ((Double, Double, Double), RNG) = {
        val (d1, rng1) = _double(rng0)
        val (d2, rng2) = _double(rng1)
        val (d3, rng3) = _double(rng2)
        ((d1, d2, d3), rng3)
    }

    def _ints(count: Int)(rng0: RNG, listSoFar:List[Int] = Nil): (List[Int], RNG) = 
        if (count == 0) (listSoFar, rng0) else {
            val (n, rng1) = _int(rng0)
            _ints(count - 1)(rng1, n::listSoFar)
        }

    def int: Rand[Int] = rng => rng.nextInt

    def nonNegativeInt: Rand[Int] = 
        map(int)(n => if (n == Int.MinValue) Int.MaxValue else n.abs)

    def double: Rand[Double] = 
        map(nonNegativeInt)(_.toDouble/Int.MaxValue.toDouble)

    def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = 
        map2(ra, rb)((_, _))

    def intDouble: Rand[(Int, Double)] = both(int, double)

    def doubleInt: Rand[(Double, Int)] = both(double, int)

    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng0 => {
        var rngCurr = rng0

        import scala.collection.mutable
        val buf = new mutable.ListBuffer[A]
        for(f <- fs){
            val (a, rngNext) = f(rngCurr)
            buf += a
            rngCurr = rngNext
        }

        (buf.toList, rngCurr)
    }

    def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng0 => {
        val (a, rng1) = f(rng0)
        g(a)(rng1)
    }

    def mapFM[A, B](s: Rand[A])(f: A => B): Rand[B] = 
        flatMap(s){a => unit(f(a))}

    def map2FM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
        flatMap(ra)(a => mapFM(rb)(b => f(a, b)))

    def ints(n: Int): Rand[List[Int]] = sequence(List.fill(n)(int))
}

case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
        val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
        val nextRNG = SimpleRNG(newSeed)
        val n = (newSeed >>> 16).toInt
        (n, nextRNG)
    }
}

