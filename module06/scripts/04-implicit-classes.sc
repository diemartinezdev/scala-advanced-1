implicit class TimesInt(i: Int) {
  def times(fn: => Unit): Unit = {
    var x = 0
    while (x < i) {
      x += 1
      fn
    }
  }
}

5 times println("hi")