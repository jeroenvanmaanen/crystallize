package org.leialearns.crystallize.util

import java.io.{FileInputStream, File}
import java.util.logging.LogManager

import grizzled.slf4j.Logger

trait LoggingConfiguration {
  val logConfigFile = "logging.properties"
  val current = new File(".", logConfigFile)
  val logConfigStream = if (current.exists()) new FileInputStream(current) else classOf[LoggingConfiguration].getResourceAsStream("/" + logConfigFile)
  val logManager: LogManager = LogManager.getLogManager
  logManager.readConfiguration(logConfigStream)

  def getLogger(category: Class[_]): Logger = {
    Logger(category)
  }
}
