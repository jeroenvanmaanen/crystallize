package org.leialearns.crystalize.interaction

import grizzled.slf4j.Logging
import org.leialearns.crystalize.item.Item

import scala.collection.immutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Encounter extends Logging {
  info("Created encounter")

  def run(responder: Actor, environment: Actor): Future[Unit] = {
    info("About to run encounter")
    Future {
      interact(responder: Actor, environment: Actor)
    }
  }

  def interact(responder: Actor, environment: Actor): Unit = {
    info("Running encounter")
    var state = immutable.Queue[Item]()
    while (true) {
      val observation = environment.nextAction()
      info(s"Observation: $observation")
      if (observation == Actor.stop) return
      if (observation != Actor.empty) {
        state = state.enqueue(observation)
        responder.provideItem(observation)
      }
      val action = responder.nextAction()
      info(s"Action: $action")
      if (action == Actor.stop) return
      if (action != Actor.empty) {
        state = state.enqueue(action)
        environment.provideItem(action)
      }
    }
  }
}
