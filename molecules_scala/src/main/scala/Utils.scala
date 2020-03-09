package molecules

import scala.concurrent.Future

object Utils {
  implicit class ChainOps[A](val value: A) extends AnyVal {
    def |>[B](f: A => B): B = f(value)
  }

  implicit class ChainNumericOps[T](val value: T) extends AnyVal {
    def zeroToNone(implicit n : Numeric[T]): Option[T] = if (value == 0) None else Some(value)
  }

  def option2future[A](option: Option[A]): Future[A] = {
    option2futureCustom[A](new Exception("No value bruh :("))(option)
  }

  def option2futureCustom[A](exception: => Exception)(option: Option[A]): Future[A] = {
    option.map(Future.successful).getOrElse(Future.failed(exception))
  }
}
