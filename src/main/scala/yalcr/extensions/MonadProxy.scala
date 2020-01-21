package yalcr.extensions

case class MonadProxy[M[_], A](x: M[A])(implicit monad: Monad[M]) {
  def flatMap[B](λ: A => M[B]): M[B] = monad.flatMap(x)(λ)
  def map[B](λ: A => B): M[B] = monad.map(x)(λ)
  def withFilter(p: A => Boolean): M[A] = monad.filter(x)(p)
}

object MonadProxy {
  type Wrapper[M[_], A] = M[A] => MonadProxy[M, A]

  implicit def via[M[_], A](implicit monad: Monad[M]): Wrapper[M, A] = x => MonadProxy(x)
}