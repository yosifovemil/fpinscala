package fpinscala.laziness

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case Empty => throw new Exception("Not enough elements in stream")
  }

  def takeWithUnfold(n: Int): Stream[A] =
    unfold( (this, n) ) {
      case (Cons(h, t), num) if num > 0 => Some((h(), (t(), num - 1)))
      case _ => None
    }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case Cons(_, t) if n == 0 => t()
    case Empty => throw new Exception("Not enough elements in stream")
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileFR(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (p(a)) cons(a, b) else b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a,b) => p(a) && b)

  def headOption: Option[A] = ???

  def toList: List[A] =
    this match {
      case Cons(h, t) => h() :: t().toList
      case _ => List.empty[A]
    }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a,b) => cons(f(a), b))

  def mapWithUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h,t) => Some((f(h()), t()))
      case _ => None
    }

  def zipWith[B](other: Stream[B])(f: (A,B) => B): Stream[B] =
    unfold( (this, other) ) {
      case (Cons(thisHead, thisTail), Cons(otherHead, otherTail)) =>
        Some(f(thisHead(), otherHead()), (thisTail(), otherTail()))
      case _ => None
    }

  def zipAll[B](s2: Stream[B]): Stream[(Option[A], Option[B])] =
    zipWithAll(s2)((_,_))

  def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
    unfold ( (this, s2) ) {
      case ( Cons(s1H, s1T), Cons(s2H, s2T) ) =>
        Some(f(Some(s1H()), Some(s2H())), (s1T(), s2T()))
      case ( Cons(s1H, s1T), Empty ) =>
        Some(f(Some(s1H()), None), (s1T(), empty))
      case ( Empty, Cons(s2H, s2T) ) =>
        Some(f(None, Some(s2H())), (empty, s2T()))
      case _ => None
    }

  def tails: Stream[Stream[A]] =
    unfold (empty[A], this) {
      case (Empty, Cons(h, t)) => Some(Cons(h, t), (Cons(h, t), t()))
      case (Cons(_, prevT), Cons(_, t)) => Some(prevT(), (prevT(), t()))
      case (Cons(_, prevT), Empty) => Some(prevT(), (prevT(), empty[A]))
      case (Empty, Empty) => None
    }

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a,b) => if (f(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((h,t) => cons(h,t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a,b) => f(a) append b)

  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  def constant[A](a: A): Stream[A] =
    Stream.cons(a, constant(a))

  val ones: Stream[Int] = Stream.cons(1, ones)
  def from(n: Int): Stream[Int] =
    Stream.cons(n, from(n + 1))

  def fibs(n1: Int = 0, n2: Int = 1): Stream[Int] =
    Stream.cons(n1, fibs(n2, n1 + n2))

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      case None => Empty
    }

  def fibsWithUnfold(n1: Int = 0, n2: Int = 1): Stream[Int] =
    unfold((n1,n2)){case (x1, x2) => Some(x1, (x2, x1 + x2))}

  def fromWithUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, (x + 1)))

  def constantWithUnfold[A](a: A): Stream[A] =
    unfold(a)(x => Some(x, a))

}