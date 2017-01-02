package org.leialearns.crystallize

import org.leialearns.crystallize.state.State

trait Propagator {
  def propagate(location: Location[_], state: State[_]): Seq[DerivedLocation[_]]
}
