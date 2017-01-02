package org.leialearns.crystallize.reader

import org.leialearns.crystallize.interaction.Actor
import org.leialearns.crystallize.item.{Category,Item}

import scala.io.Source

class TokenSource(_category: Category, _lines: Iterator[String]) extends Actor {
  def this(_category: Category, _source: Source) {
    this(_category, _source.getLines())
  }
  def this(_category: Category, _fileName: String) {
    this(_category, Source.fromFile(_fileName, "UTF-8"))
  }
  val category = _category
  val lines = _lines
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
