package org.leialearns.crystalize

import scala.concurrent.Future

trait Derived[T] {
  def derive(location: DerivedLocation[T], state: State[_]): Future[T]
}
