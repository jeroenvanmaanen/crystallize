package org.leialearns.crystallize.interaction

import java.lang.invoke.MethodHandles
import org.leialearns.crystallize.event.History
import org.leialearns.crystallize.item.Category
import org.leialearns.crystallize.reader.TokenSource
import org.leialearns.crystallize.util.{Dump, LoggingConfiguration}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.util.Success

object FileExample {
  val logger = (new Object with LoggingConfiguration).getLogger(MethodHandles.lookup().lookupClass())

  def main(args: Array[String]): Unit = {
    logger.info("FileExample")
    var limit: Option[Long] = None
    var optIndex = 0
    if (args(0) == "-l") {
      limit = Some(args(1).toLong)
      optIndex += 2
    }
    val file = args(optIndex)
    try {
      run(file, limit)
    } catch {
      case e: Throwable =>
        e.printStackTrace()
        logger.error("Aborted", e)
    }
  }

  def run(file: String, limit: Option[Long]) = {
    logger.info("Run");
    val observation = Category.getCategory("observation")
    val environment = new TokenSource(observation, file)
    logger.info(s"Environment: ${environment}");
    val actor = new NullActor(limit)
    logger.info(s"Actor: ${actor}");

    val history = new History()
    logger.info(s"History: ${history}");
    val encounter = new Encounter(history)
    logger.info(s"Encounter: ${encounter}");
    val runDone = encounter.run(actor, environment)
    runDone onFailure {
      case exception: Throwable =>
        exception.printStackTrace()
        logger.warn("Exception while running encounter", exception)
    }
    while (!runDone.isCompleted) {
      Thread.sleep(500l)
    }
    logger.info("Complete")
    if (logger.isDebugEnabled) {
      logger.info("Dump")
      logger.debug("Debug")
      Dump.dump("", history.lastSnapshot._2).foreach {
        line => logger.debug(line)
      }
    }
  }
}
