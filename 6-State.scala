package state

case class State[S, +A](run: S => (A, S)) {
    def _map[B](f: A => B): State[S, B] = 
        State(s => {
            val (a, s1) = run(s)
            (f(a), s1)
        })

    def map[B](f: A => B): State[S, B] = flatMap(a => State.unit(f(a)))

    def flatMap[B](f: A => State[S, B]): State[S, B] = {
        State(s => {
            val (a, s1) = run(s)
            f(a).run(s1)
        })
    }
}

object State {
    def unit[S, A](a: A) : State[S, A] = State(s => (a, s))

    def map2[S, A, B, C](sa: State[S, A], sb: State[S, B])(f: (A, B) => C): State[S, C] = 
        sa.flatMap(a => sb.map(b => f(a, b)))

    // implement sequence later.
}
