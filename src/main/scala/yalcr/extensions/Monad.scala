package yalcr.extensions

trait Monad[M[_]] {
  def flatMap[A, B](x: M[A])(λ: A => M[B]): M[B]
  def map[A, B](x: M[A])(λ: A => B): M[B] = flatMap(x)(a => ret(λ(a)))
  def ret[A](x: A): M[A]

  def filter[A](x: M[A])(p: A => Boolean): M[A]
}

