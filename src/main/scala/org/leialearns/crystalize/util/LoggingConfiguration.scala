package org.leialearns.crystalize.util

import java.util.logging.LogManager

import grizzled.slf4j.Logger

trait LoggingConfiguration {
  val logConfig = "/logging.properties"
  System.err.println("Configuring logging using log directory: " + logConfig)
  val logConfigStream = classOf[LoggingConfiguration].getResourceAsStream(logConfig)
  val logManager: LogManager = LogManager.getLogManager
  logManager.readConfiguration(logConfigStream)

  def getLogger(category: Class[_]): Logger = {
    Logger(category)
  }
}
