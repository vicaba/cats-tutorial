package rockthejvm

// https://www.youtube.com/watch?v=_afyiQB7N0Y

object CatsOverview {

  /**
   * Semigroup -> Monoid
   *
   * Semigroupal           Functor -> Applicative -> Monad
   *                                \              /
   *                                  FlatMap ---->
   *
   */

  /**
   * Abstraction to combine elements of type A
   * @tparam A
   */
  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  /**
   * If you combine an element with empty, the result is the element.
   * E.g., in the case of integer multiplication, the empty element would be the number 1
   * @tparam A
   */
  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_]] extends Functor[F] {
    def pure[A](a: A): F[A]
  }

  trait FlatMap[F[_]] extends Functor[F] {
    def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait Monad[F[_]] extends Applicative[F] with FlatMap[F] {
    override def map[A, B](fa: F[A])(f: A => B): F[B] =
      flatMap(fa)(a => pure(f(a)))
  }

  /**
   * Cartesian product
   * @tparam F
   */
  trait Semigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  trait Apply[F[_]] {
    def ap[A, B](fab: F[A => B], fa: F[A]): F[B]
  }

}
