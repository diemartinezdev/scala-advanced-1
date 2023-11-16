trait JSONWrite[T] {
  def toJsonString(item: T): String
}

def jsonify[T: JSONWrite](item: T): String =
  implicitly[JSONWrite[T]].toJsonString(item)


implicit object StrJsonWrite extends JSONWrite[String] {
  override def toJsonString(item: String) = s""""$item""""
}

jsonify("Hola")

implicit object DoubleJsonWrite extends JSONWrite[Double] {
  override def toJsonString(item: Double) = item.toString
}

jsonify(2.0)

implicit object BooleanJsonWrite extends JSONWrite[Boolean] {
  override def toJsonString(item: Boolean) = item.toString
}


implicit def listJsonWrite[T: JSONWrite] = new JSONWrite[List[T]] {
  override def toJsonString(xs: List[T]): String = {
    xs.map(x => jsonify(x)).mkString("[", ",", "]")
  }
}

jsonify("Hello")

jsonify(List("Hello", "World"))
jsonify(List(1.0, 2.0, 3.0))
jsonify(List(true,false))


implicit object IntJsonfify extends JSONWrite[Int] {
  override def toJsonString(item: Int) = item.toString
}

jsonify(List(1,2,3))

implicit def mapJSONWrite[T: JSONWrite] = new JSONWrite[Map[String, T]] {
  def toJsonString(m: Map[String, T]): String = {
    val pairs = for ((k, v) <- m) yield
      s"${jsonify(k)}: ${jsonify(v)}"
    pairs.mkString("{\n  ", ",\n  ", "\n}")
  }
}

jsonify(Map(
  "hello" -> List("hello", "world"),
  "goodbye" -> List("goodbye", "cruel", "world")
))
