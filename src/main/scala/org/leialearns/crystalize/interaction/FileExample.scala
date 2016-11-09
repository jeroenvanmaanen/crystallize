package org.leialearns.crystalize.interaction

import org.leialearns.crystalize.Crystal
import org.leialearns.crystalize.item.Category
import org.leialearns.crystalize.model.{MaxDepth, ExtensiblePropagator}
import org.leialearns.crystalize.reader.TokenSource
import org.leialearns.crystalize.util.{Dump, LoggingConfiguration}

object FileExample {
  def main(args:Array[String]) = {
    val logger = (new Object with LoggingConfiguration).getLogger(classOf[TokenSource])
    logger.info("FileExample")
    val observation = Category.getCategory("observation")
    val environment = new TokenSource(observation, args(0))
    val actor = new NullActor(Some(10000l))

    val crystal = new Crystal(new ExtensiblePropagator() :: MaxDepth.MAX_DEPTH :: Nil)
    val encounter = new Encounter(crystal)
    val done = encounter.run(actor, environment)
    while (!done.isCompleted) {
      Thread.sleep(500l)
    }

    val finalState = crystal.head.get()
    for (line <- Dump.dump("", finalState)) {
      logger.debug(line)
    }
  }
}
