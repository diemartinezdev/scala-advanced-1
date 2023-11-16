val intNumT = implicitly[Numeric[Int]]
intNumT.times(5, 8)

val doubleNumT = implicitly[Numeric[Double]]
doubleNumT.times(5.0, 8.0)

// but can't compile this one:
//
// val stringNumT = implicitly[Numeric[String]]

case class Cell[T](item: T) {
  def *[U: Numeric](other: Cell[U])(implicit ev: T =:= U): Cell[U] = {
    val numClass = implicitly[Numeric[U]]
    Cell(numClass.times(this.item, other.item))
  }
}

List(2,3,4).sum

// can still make Cells of any type
val stringCell = Cell("hello")
val intCell = Cell(6)

// but can only multiply Numerics
intCell * Cell(7)

// and not e.g. Strings...
// stringCell * Cell("there")