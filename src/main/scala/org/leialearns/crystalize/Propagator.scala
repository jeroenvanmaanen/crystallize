package org.leialearns.crystalize

import org.leialearns.crystalize.state.State

trait Propagator {
  def propagate(location: Location[_], state: State[_]): Seq[DerivedLocation[_]]
}
