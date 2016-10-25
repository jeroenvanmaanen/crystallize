package org.leialearns.crystalize.interaction

import org.leialearns.crystalize.item.Item

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Encounter {
  def run(responder: Actor, environment: Actor): Future[Unit] = {
    Future {
      interact(responder: Actor, environment: Actor)
    }
  }

  def interact(responder: Actor, environment: Actor): Unit = {
    var state = immutable.Queue[Item]()
    while (true) {
      val observation = environment.nextAction()
      if (observation == Actor.stop) return
      if (observation != Actor.empty) {
        state = state.enqueue(observation)
        responder.provideItem(observation)
      }
      val action = responder.nextAction()
      if (action == Actor.stop) return
      if (action != Actor.empty) {
        state = state.enqueue(action)
        environment.provideItem(action)
      }
    }
  }
}
