package org.leialearns.crystallize.util

import java.io.{FileInputStream, File}
import java.util.logging.LogManager

import grizzled.slf4j.Logger

trait LoggingConfiguration {
  private val logConfigFile = "logging.properties"
  private val current = new File(".", logConfigFile)
  private val logConfigStream = if (current.exists()) new FileInputStream(current) else classOf[LoggingConfiguration].getResourceAsStream("/" + logConfigFile)
  val logManager: LogManager = LogManager.getLogManager
  logManager.readConfiguration(logConfigStream)

  def getLogger(category: Class[_]): Logger = {
    Logger(category)
  }
}
