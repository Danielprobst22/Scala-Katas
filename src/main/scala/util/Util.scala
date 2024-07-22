package util

object Util {
  extension (o: Option.type) {
    def and[A](
      o1: Option[A],
      o2: Option[A],
      f: (A, A) => A
    ): Option[A] = (o1, o2) match {
      case (Some(v1), Some(v2)) => Some(f(v1, v2))
      case _ => None
    }
  }
}
