import scala.util.parsing.json._
import scala.math.ScalaNumber

object Json {
	JSON.globalNumberParser = s => BigDecimal(s)
	
	trait Obj
	
	def serialize: Obj => String = _.toString
	def deserialize: String => Option[Json.Obj] = s => JSON.parseFull(s) map foo
	
	type Serializable[A] = PartialFunction[A,Json.Obj]
	type Deserializable[A] = PartialFunction[Json.Obj,A]
	
	case class Str(value: String) extends Obj {
		override def toString = '"' + value.replace("\\", "\\\\").replace("\"", "\\\"") + '"'
	}
	case class Number(value: BigDecimal) extends Obj {
		def this(value: Int) = this(BigDecimal(value))
		override def toString = value toString
	}
	case class Arr(value: Seq[Obj]) extends Obj {
		override def toString = value mkString ("[", ", ", "]")
	}
	case class Object(value: (String,Obj)*) extends Obj {
		override def toString = value map {case (l,r) => '"' + l + '"' + ": " + r} mkString ("{\n", ",\n", "\n}")
	}
	
	object Object {
		def unapplyseq(json: JSONType): Seq[(String,Json.Obj)] = json match {
			case x:JSONArray => Seq()
			case JSONObject(m) => m map { case (a,b) => (a, foo(b)) } toSeq
		}
	}
	
	val foo: Any => Json.Obj = _ match {
		case i: BigDecimal => Number(i)
		case s: String => Str(s)
		case a: List[_] => Arr(a map foo toSeq)
		case m: Map[String,_] => Object(m map { case (a,b) => (a, foo(b)) } toSeq: _*)
	}
	
	implicit val StringIsSerializable: String => Json.Str = Str(_)
	implicit val StringIsDeserializable: Deserializable[String] = {
		case Json.Str(value) => value
	}
	
	implicit val IntIsSerializable: Int => Json.Number = Number(_)
	implicit val BigDEcimalIsSerializable: BigDecimal => Json.Number = Number(_)
	implicit val IntIsDeserializable: Deserializable[Int] = {
		case Number(value) if value.isValidInt => value.intValue
	}
	implicit val BigDecimalIsDeserializable: Deserializable[BigDecimal] = {
		case Number(value) => value
	}
	
	implicit def SeqIsSerializable[T <% Json.Obj](s: Seq[T]): Json.Obj = Arr(s map {a => implicitly[Obj](a)})
	implicit def SeqIsDeserializable[T](s: Json.Obj)(implicit i: Json.Obj => T): Seq[T] = s match {
		case Arr(value) => value map { i }
	}
	
	implicit def string2Foo(name: String) = new {
		def ->[B <% Json.Obj](y: B): (String,Json.Obj) = (name,y)
	}
	
	type T = (String,Json.Obj)
	implicit def Tuple1IsSerializable: Tuple1[T] => Json.Obj = t => Json.Object(t._1)
	implicit def Tuple2IsSerializable: Tuple2[T,T] => Json.Obj = t => Json.Object(t._1, t._2)
	implicit def Tuple3IsSerializable: Tuple3[T,T,T] => Json.Obj = t => Json.Object(t._1, t._2, t._3)
	implicit def Tuple4IsSerializable: Tuple4[T,T,T,T] => Json.Obj = t => Json.Object(t._1, t._2, t._3, t._4)
}