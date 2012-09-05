import scala.util.parsing.json._
import scala.math.ScalaNumber
import scala.collection.immutable.Set

object Json {
	JSON.globalNumberParser = s => BigDecimal(s)
	
	class O(var m: Map[String,Json.Obj] = null, var f: String = null) {
		override def equals(obj: Any) = {
	  	val ret = obj match { 
	  		case x: String if m == null => false
	    	case x: String if m.keySet contains x => f = x; true
	    	case _ => true 
	  	}
			//println("Oequals: " + obj + " = " + ret)
			ret
	  }
	}
	object O {
		def unapply(s: O): Option[String] = Some(s).asInstanceOf[Option[String]]
	}
	
	class Obj extends O
	
	def serialize: Obj => String = _.toString
	def deserialize[T: Deserializable](s: String): Option[T] = try {
		JSON.parseFull(s) map foo collect {implicitly}
	} catch {
		case e: scala.MatchError => None
	}
	
	type Serializable[A] = PartialFunction[A,Json.Obj]
	type Deserializable[A] = PartialFunction[O,A]
	
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
	class Object(val value: Map[String,Obj]) extends Obj {
		override def toString = value filter { _._2 != null } map {case (l,r) => '"' + l + '"' + ": " + r} mkString ("{\n", ",\n", "\n}")
	}
	
	
	
	object Object {
		def unapplySeq(elems: Json.Object): Option[Seq[O]] = {
			val seq = new collection.SeqProxy[O] {
				val self = elems.value.map { a => 
						val y = a._2 match {
							case Str(s) => Str(s);
							case Number(n) => Number(n)
							case Arr(a) => Arr(a)
							case o: Object => new Object(o.value)
						}
						y.m = elems.value
						y.f = a._1
						y
					} toSeq
				override def apply(i: Int) = if (i >= size) super.apply(size-1) else super.apply(i)
				override def lengthCompare(i: Int) = 0
			}
			Some(seq)
		}
	}

	object MockO extends O {
		override def equals(a: Any) = true
		override def toString = "MockO"
	}
	
	val foo: Any => Json.Obj = _ match {
		case i: BigDecimal => Number(i)
		case s: String => Str(s)
		case a: List[_] => Arr(a map foo toSeq)
		case m: Map[String,_] => new Object(m map { case (a,b) => (a, foo(b)) })
	}
	
	implicit val StringIsSerializable: Serializable[String] = { case x => Str(x) }
	implicit val IntIsSerializable: Serializable[Int] = { case x => Number(x) }
	implicit val BigDecimalIsSerializable: Serializable[BigDecimal] = { case x => Number(x) }
	
	implicit def SeqIsSerializable[T: Serializable]: Serializable[Seq[T]] = { case x => Arr(x map {a => implicitly[Json.Obj](a)}) }
	implicit def OptionalIsSerializable[T: Serializable]: Serializable[Option[T]] = {
		case Some(x) => x
		case None => null
	}
	
	implicit val StringIsDeserializable: Deserializable[String] = {
		case Json.Str(value) => value
		case x:O if x.m != null && x.f != null && StringIsDeserializable.isDefinedAt(x.m(x.f)) => StringIsDeserializable(x.m(x.f))
	}
	
	implicit val IntIsDeserializable: Deserializable[Int] = {
		case Number(value) if value.isValidInt => value.intValue
		case x:O if x.m != null && x.f != null && IntIsDeserializable.isDefinedAt(x.m(x.f)) => IntIsDeserializable(x.m(x.f))
	}
	
	implicit val BigDecimalIsDeserializable: Deserializable[BigDecimal] = {
		case Number(value) => value
		case x:O if x.m != null && x.f != null && BigDecimalIsDeserializable.isDefinedAt(x.m(x.f)) => BigDecimalIsDeserializable(x.m(x.f))
	}
	
	implicit def SeqIsDeserializable[T](implicit e: Deserializable[T]): Deserializable[Seq[T]] = {
		case Arr(value) => value collect { case x if e.isDefinedAt(x) => implicitly[T](x) }
	}
	
	implicit def OptionalIsDeserializable[T](implicit i: Deserializable[T]): Deserializable[Option[T]] = {
		case x:Json.Obj if i.isDefinedAt(x) => Some(i(x))
		case o: O if o.m != null && o.f != null && OptionalIsDeserializable(i).isDefinedAt(o.m(o.f)) => OptionalIsDeserializable(i)(o.m(o.f))
		case o: O => None
	}
	
	implicit val OptionalStringIsDeserializable: Deserializable[Option[String]] = OptionalIsDeserializable
	implicit val OptionalIntIsDeserializable: Deserializable[Option[Int]] = OptionalIsDeserializable
	implicit val OptionalBigDecimalIsDeserializable: Deserializable[Option[BigDecimal]] = OptionalIsDeserializable
	
	implicit def string2pair(name: String) = new {
		def ->[B <% Json.Obj](y: B): (String,Json.Obj) = (name,y)
	}
	
	private type T = (String,Json.Obj)
	implicit def Tuple1IsSerializable: Tuple1[T] => Json.Obj = t => new Json.Object(Map(t._1))
	implicit def Tuple2IsSerializable: Tuple2[T,T] => Json.Obj = t => new Json.Object(Map(t._1, t._2))
	implicit def Tuple3IsSerializable: Tuple3[T,T,T] => Json.Obj = t => new Json.Object(Map(t._1, t._2, t._3))
	implicit def Tuple4IsSerializable: Tuple4[T,T,T,T] => Json.Obj = t => new Json.Object(Map(t._1, t._2, t._3, t._4))
}