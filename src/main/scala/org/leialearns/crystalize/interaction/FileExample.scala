package org.leialearns.crystalize.interaction

import org.leialearns.crystalize.item.Category
import org.leialearns.crystalize.reader.TokenSource
import org.leialearns.crystalize.util.LoggingConfiguration

object FileExample {
  def main(args:Array[String]) = {
    val logger = (new Object with LoggingConfiguration).getLogger(classOf[TokenSource])
    logger.info("FileExample")
    val observation = Category.getCategory("observation")
    val environment = new TokenSource(observation, args(0))
    val actor = new NullActor
    val encounter = new Encounter
    val done = encounter.run(actor, environment)
    while (!done.isCompleted) {
      Thread.sleep(500l)
    }
  }
}
