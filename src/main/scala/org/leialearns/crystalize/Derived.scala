package org.leialearns.crystalize

import org.leialearns.crystalize.state.State

import scala.concurrent.Future

trait Derived[T] {
  def derive(location: DerivedLocation[T], state: State[_]): Future[T]
}
