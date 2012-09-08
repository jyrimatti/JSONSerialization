import scala.util.parsing.json._
import scala.math.ScalaNumber
import scala.collection.immutable.Set

object Json {
	JSON.globalNumberParser = s => BigDecimal(s)
	
	abstract class O(var m: Map[String,Json.O] = null, var f: String = null) {
		override def equals(obj: Any) = {
	  	obj match { 
	    	case x: String => f = x; true 
	  	}
	  }
		def toJSON: Any
	}
	object O {
		def unapply(s: O): Option[String] = Some(s).asInstanceOf[Option[String]]
	}
	
	def serialize: O => String = _.toJSON.toString
	def deserialize[T: Deserializable](s: String): Option[T] = try {
		JSON.parseFull(s) map foo map {implicitly}
	} catch {
		case e: scala.MatchError => None
		//case e: scala.MatchError => throw e
	}
	
	type Serializable[A] = PartialFunction[A,O]
	type Deserializable[A] = PartialFunction[O,A]
	
	trait JsonExtractor[T,OT <: {def value: T}] {
		def unapply(s: O): Option[T] = s match {
			case s: OT if s.m == null => Some(s.value)
			case o: O if o.m.isDefinedAt(o.f) => unapply(o.m(o.f))
			case _ => None
		}
	}
	
	class Str(val value: String) extends O {
		def toJSON = value
	}
	object Str extends JsonExtractor[String,Str]
	
	class Number(val value: BigDecimal) extends O {
		def this(value: Int) = this(BigDecimal(value))
		def toJSON = value
	}
	object Number extends JsonExtractor[BigDecimal,Number]
	
	class Arr(val value: Seq[O]) extends O {
		def toJSON = JSONArray(value map {_.toJSON} toList)
	}
	object Arr extends JsonExtractor[Seq[O],Arr]
	
	class Object(val value: Map[String,O]) extends O {
		def toJSON = JSONObject(value filter { _._2 != null } map {
			case (k,v) => (k, v.toJSON) 
		})
	}
	
	object Object {
		def unapplySeq(elems: Json.Object): Option[Seq[O]] = {
			val seq: Seq[O] = new collection.SeqProxy[O] {
				def mock = new O {
					def toJSON = throw new RuntimeException()
					m = elems.value
				}
				val self = elems.value.map { a => 
						val y = a._2 match {
							case s: Str => new Str(s.value);
							case n: Number => new Number(n.value)
							case a: Arr => new Arr(a.value)
							case o: Object => new Object(o.value)
						}
						y.m = elems.value
						y
					} toSeq
				override def apply(i: Int) = if (i >= size) mock else super.apply(i)
				override def lengthCompare(i: Int) = 0
			}
			Some(seq)
		}
	}

	val foo: Any => Json.O = _ match {
		case i: BigDecimal => new Number(i)
		case s: String => new Str(s)
		case a: List[_] => new Arr(a map foo toSeq)
		case m: Map[String,_] => new Object(m map { case (a,b) => (a, foo(b)) })
	}
	
	implicit val StringIsSerializable: Serializable[String] = { case x => new Str(x) }
	implicit val IntIsSerializable: Serializable[Int] = { case x => new Number(x) }
	implicit val BigDecimalIsSerializable: Serializable[BigDecimal] = { case x => new Number(x) }
	
	implicit def SeqIsSerializable[T: Serializable]: Serializable[Seq[T]] = { case x => new Arr(x map {a => implicitly[Json.O](a)}) }
	implicit def OptionalIsSerializable[T: Serializable]: Serializable[Option[T]] = {
		case Some(x) => x
		case None => null
	}
	
	implicit val StringIsDeserializable: Deserializable[String] = {
		case x@Json.Str(value) => value
	}
	
	implicit val IntIsDeserializable: Deserializable[Int] = {
		case x@Number(value) if x.m == null && value.isValidInt => value.intValue
		case x:O if x.m != null && x.f != null && IntIsDeserializable.isDefinedAt(x.m(x.f)) => IntIsDeserializable(x.m(x.f))
		case x => throw new RuntimeException(x)
	}
	
	implicit val BigDecimalIsDeserializable: Deserializable[BigDecimal] = {
		case x@Number(value) if x.m == null => value
		case x:O if x.m != null && x.f != null && BigDecimalIsDeserializable.isDefinedAt(x.m(x.f)) => BigDecimalIsDeserializable(x.m(x.f))
	}
	
	implicit def s[T](x: Json.O)(implicit e: Deserializable[T]): Seq[T] = SeqIsDeserializable(e)(x)
	def SeqIsDeserializable[T](implicit e: Deserializable[T]): Deserializable[Seq[T]] = {
		case Arr(value) => value map { case x if e.isDefinedAt(x) => e(x) }
	}
	
	implicit def o[T](x: Json.O)(implicit i: Deserializable[T]): Option[T] = OptionIsDeserializable(i)(x)
	def OptionIsDeserializable[T](implicit i: Deserializable[T]): Deserializable[Option[T]] = {
		case x: O if x.m == null && i.isDefinedAt(x) => Some(i(x))
		case x: O if x.m != null && x.f != null && x.m.isDefinedAt(x.f) && OptionIsDeserializable(i).isDefinedAt(x.m(x.f)) => OptionIsDeserializable(i)(x.m(x.f))
		case _ => None
	}
	
	implicit val OptionalStringIsDeserializable: Deserializable[Option[String]] = OptionIsDeserializable
	implicit val OptionalIntIsDeserializable: Deserializable[Option[Int]] = OptionIsDeserializable
	implicit val OptionalBigDecimalIsDeserializable: Deserializable[Option[BigDecimal]] = OptionIsDeserializable
	
	implicit val SeqStringIsDeserializable: Deserializable[Seq[String]] = SeqIsDeserializable
	implicit val SeqIntIsDeserializable: Deserializable[Seq[Int]] = SeqIsDeserializable
	implicit val SeqBigDecimalIsDeserializable: Deserializable[Seq[BigDecimal]] = SeqIsDeserializable
	
	implicit def string2pair(name: String) = new {
		def ->[B <% O](y: B): (String,Json.O) = (name,y)
	}
	
	private type T = (String,Json.O)
	implicit def Tuple1IsSerializable: Tuple1[T] => Json.O = t => new Json.Object(Map(t._1))
	implicit def Tuple2IsSerializable: Tuple2[T,T] => Json.O = t => new Json.Object(Map(t._1, t._2))
	implicit def Tuple3IsSerializable: Tuple3[T,T,T] => Json.O = t => new Json.Object(Map(t._1, t._2, t._3))
	implicit def Tuple4IsSerializable: Tuple4[T,T,T,T] => Json.O = t => new Json.Object(Map(t._1, t._2, t._3, t._4))
}