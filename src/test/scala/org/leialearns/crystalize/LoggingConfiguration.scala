package org.leialearns.crystalize

import java.util.logging.LogManager

import grizzled.slf4j.Logger

trait LoggingConfiguration {
  val logDir = "/logging.properties"
  System.err.println("Configuring logging using log directory: " + logDir)
  val logConfigStream = classOf[LoggingConfiguration].getResourceAsStream(logDir)
  val logManager: LogManager = LogManager.getLogManager
  logManager.readConfiguration(logConfigStream)

  def getLogger(category: Class[_]): Logger = {
    Logger(category)
  }
}
