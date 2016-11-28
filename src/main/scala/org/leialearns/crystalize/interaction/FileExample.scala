package org.leialearns.crystalize.interaction

import org.leialearns.crystalize.Crystal
import org.leialearns.crystalize.item.Category
import org.leialearns.crystalize.model.{ExtensiblePropagator, MaxDepth}
import org.leialearns.crystalize.reader.TokenSource
import org.leialearns.crystalize.util.{Dump, LoggingConfiguration}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Promise
import scala.util.Success

object FileExample {
  val logger = (new Object with LoggingConfiguration).getLogger(classOf[TokenSource])

  def main(args: Array[String]) = {
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
    val observation = Category.getCategory("observation")
    val environment = new TokenSource(observation, file)
    val actor = new NullActor(limit)

    val crystal = new Crystal(new ExtensiblePropagator() :: MaxDepth.MAX_DEPTH :: Nil)
    val encounter = new Encounter(crystal)
    val runDone = encounter.run(actor, environment)
    runDone onFailure {
      case exception: Throwable =>
        exception.printStackTrace()
        logger.warn("Exception while running encounter", exception)
    }
    while (!runDone.isCompleted) {
      Thread.sleep(500l)
    }

    val dumpDone = Promise[Unit]()
    crystal.head.get().get(MaxDepth.MAX_DEPTH_LOCATION) onComplete {
      case result =>
        result match {
          case Success(lastMaxDepth) => logger.info(s"Last max depth: $lastMaxDepth")
          case _ => ()
        }
        val finalState = crystal.head.get()
        for (line <- Dump.dump("", finalState)) {
          logger.debug(line)
        }
        dumpDone.success()
    }
    while (!dumpDone.isCompleted) {
      Thread.sleep(500l)
    }
  }
}
