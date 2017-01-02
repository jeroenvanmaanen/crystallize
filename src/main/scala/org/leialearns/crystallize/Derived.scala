package org.leialearns.crystallize

import org.leialearns.crystallize.state.State

import scala.concurrent.Future

trait Derived[T] {
  def derive(location: DerivedLocation[T], state: State[_]): Future[T]
}
