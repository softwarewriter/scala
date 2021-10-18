package no.jergan.scrapbook

import ch.qos.logback.classic.encoder.PatternLayoutEncoder
import ch.qos.logback.classic.spi.ILoggingEvent
import ch.qos.logback.classic.{AsyncAppender, Level, LoggerContext}
import ch.qos.logback.core.{Appender, ConsoleAppender}
import org.slf4j.LoggerFactory


object Logback {
  org.log4s.getLogger

  val context: LoggerContext = LoggerFactory.getILoggerFactory.asInstanceOf[LoggerContext]

  def apply(rootLevel: Level): Unit = {
    context.reset()
    val logEncoder = new PatternLayoutEncoder()
    logEncoder.setContext(context)
    logEncoder.setPattern("%d{HH:mm:ss.SSS} [%thread] %-5level %logger{36} - %msg%n")
    logEncoder.start()

    val consoleAppender = new ConsoleAppender[ILoggingEvent]()
    consoleAppender.setContext(context)
    consoleAppender.setName("console")
    consoleAppender.setEncoder(logEncoder)
    consoleAppender.start()

    val asyncConsoleAppender = new AsyncAppender()
    asyncConsoleAppender.setContext(context)
    asyncConsoleAppender.addAppender(consoleAppender)
    asyncConsoleAppender.start()

    val rootLogger = context.getLogger("ROOT")
    rootLogger.addAppender(asyncConsoleAppender)
    rootLogger.setLevel(rootLevel)
  }

}
