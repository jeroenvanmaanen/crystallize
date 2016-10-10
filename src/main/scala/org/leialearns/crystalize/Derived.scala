package org.leialearns.crystalize

import scala.concurrent.Future

trait Derived[T] {
  def derive(state: State[_]): Future[T]
  def anchors(): Seq[Location[_]]
  def propagate(location: Location[_], state: State[_]): Seq[Location[_]]
}
