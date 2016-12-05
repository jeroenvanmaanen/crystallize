package org.leialearns.crystalize.util

import java.util.concurrent.atomic.AtomicReference

import scala.collection.immutable

object Intern {
  private val internalized = new AtomicReference[immutable.HashMap[AnyRef,Internalizable]]()
  internalized.set(immutable.HashMap.empty)

  def internalize[T <: Internalizable](thing: T): T = {
    val key = thing.equivalenceKey
    var result: Option[Internalizable] = None
    do {
      val oldInternalized = internalized.get()
      result = oldInternalized.get(key)
      result match {
        case Some(internal) => internal
        case _ =>
          val newInternalized = oldInternalized + ((key, thing))
          internalized.compareAndSet(oldInternalized, newInternalized)
      }
    } while (result.isEmpty)
    thing.getClass.cast(result.get)
  }
}
