package org.leialearns.crystalize

trait Propagator {
  def propagate(location: Location[_], state: State[_]): Seq[DerivedLocation[_]]
}
