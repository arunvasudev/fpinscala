def fib(n: Int) = {
    def go(a: Int, b: Int, n1: Int): Int = if (n1 == n) a else go(b, a+b, n1+1)
    go(1, 1, 1)
}
