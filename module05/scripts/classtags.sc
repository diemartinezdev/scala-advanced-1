import scala.reflect._

val s: String = "hello"

classOf[String]

val stringClassTag = classTag[String]

def getClassTag[T: ClassTag](x: T): ClassTag[T] = classTag[T]

s.getClass()

val ct = getClassTag(s)

val cts = classTag[String]

cts.newArray(10)

def isA[T: ClassTag](x: Any): Boolean = x match {
  case _: T => true
  case _ => false
}

isA[Int](7)
isA[Int]("hello")

isA[Map[String, Int]](List(1,2,3))
isA[Map[String, Int]](Map("hello" -> 2)) // true
isA[Map[String, Int]](Map("hello" -> "foo")) // true :-(

import reflect.runtime.universe._

val tt = typeTag[Map[String, Int]]

val theType = tt.tpe

theType.typeArgs

case class Tagged[A](value: A)(implicit val tag: TypeTag[A])
val taggedMap1 = Tagged(Map(1 -> "one", 2 -> "two"))
val taggedMap2 = Tagged(Map(1 -> 1, 2 -> 2))
def taggedIsA[A, B](x: Tagged[Map[A, B]]): Boolean = x.tag.tpe match {
  case t if t =:= typeOf[Map[Int, String]] => true
  case _ => false
}
taggedIsA(taggedMap1) // true
taggedIsA(taggedMap2) // false

theType.=:=(typeTag[Map[String, String]].tpe)