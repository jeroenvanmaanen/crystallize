package org.leialearns.crystallize.reader

import org.leialearns.crystallize.interaction.Actor
import org.leialearns.crystallize.item.{Category,Item}

import scala.io.Source

class TokenSource(val category: Category, val lines: Iterator[String]) extends Actor {
  def this(category: Category, source: Source) {
    this(category, source.getLines())
  }
  def this(category: Category, fileName: String) {
    this(category, Source.fromFile(fileName, "UTF-8"))
  }
  var tokens: Iterator[Item] = Nil.iterator

  override def provideItem(item: Item): Unit = {
    // empty
  }

  override def nextAction(): Item = {
    while (tokens.isEmpty && lines.nonEmpty) {
      tokens = getTokens(lines.next())
    }
    if (tokens.nonEmpty) tokens.next() else Actor.stop
  }

  def getTokens(line: String): Iterator[Item] = {
    for (part <- "[A-Za-z0-9_]+|[^\\s]".r findAllIn line) yield Item.getItem(category, part)
  }
}
