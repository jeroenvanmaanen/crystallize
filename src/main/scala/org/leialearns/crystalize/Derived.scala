package org.leialearns.crystalize

import scala.concurrent.Future

trait Derived[T] {
  def derive(location: DerivedLocation[T], state: State[_]): Future[T]
  def anchors(location: DerivedLocation[T], state: State[_]): Seq[Location[_]]
  def propagate(location: Location[_], state: State[_]): Seq[Location[_]]
}
