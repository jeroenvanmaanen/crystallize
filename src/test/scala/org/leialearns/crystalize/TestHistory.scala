package org.leialearns.crystalize

import org.scalatest.FunSuite

class TestHistory extends FunSuite {
  test("History") {
    val history = new History
    history.add(5, 100)
    history.add(10, "hi")
    history.remove(15)
    history.add(20, true)
    assert(Some(true) == history.get(22))
    assert(Some(true) == history.get(20))
    assert(None == history.get(19))
    assert(None == history.get(16))
    assert(None == history.get(15))
    assert(Some("hi") == history.get(14))
    assert(Some("hi") == history.get(13))
    assert(Some("hi") == history.get(10))
    assert(Some(100) == history.get(9))
    assert(Some(100) == history.get(5))
    assert(None == history.get(4))
    assert(None == history.get(0))
    assert(None == history.get(-1))
  }
}
