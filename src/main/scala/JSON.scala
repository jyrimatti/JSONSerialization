import scala.util.parsing.json._
import scala.math.ScalaNumber
import scala.collection.immutable.Set

import collection.mutable

object Json {
	abstract class O(var props: mutable.Buffer[String] = mutable.ArrayBuffer(), var m: Map[String,Json.O] = null, var f: String = null) {
		override def equals(obj: Any) = {
	  	obj match { 
	    	case x: String => f = x; props += x; true 
	  	}
	  }
		def toJSON: Any
	}
	object p {
		def unapply(s: O): Option[String] = Some(s).asInstanceOf[Option[String]]
	}
	
	implicit def p1[T,E1: Deserializable](d: O => (E1) => T): Deserializable[T] = p2(d.asInstanceOf[O => (E1,String) => T])
	implicit def p2[T,E1: Deserializable,E2: Deserializable](d: O => (E1,E2) => T): Deserializable[T] = {
		case o: Json.Object => {
			val f = d(o)
			(o.props map o.value) match {
				case Seq(p1) => f.asInstanceOf[(E1)=>T](p1)
				case Seq(p1,p2) => f.asInstanceOf[(E1,E2)=>T](p1,p2)
			}
		}
	}
	
	def serialize: O => String = {
		val numberParser = JSON.perThreadNumberParser
		try {
			JSON.perThreadNumberParser = s => BigDecimal(s)
			_.toJSON.toString
		} finally {
			JSON.perThreadNumberParser = numberParser
		}
	}
	
	def deserialize[T: Deserializable](s: String): Option[T] = {
		val numberParser = JSON.perThreadNumberParser
		try {
			JSON.perThreadNumberParser = s => BigDecimal(s)
			JSON.parseFull(s) map convertFromRaw map {implicitly}
		} catch {
			case e: scala.MatchError => None
			//case e: scala.MatchError => throw e
		} finally {
			JSON.perThreadNumberParser = numberParser
		}
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
		lazy val toJSON = value
	}
	object Str extends JsonExtractor[String,Str]
	
	class Number(val value: BigDecimal) extends O {
		def this(value: Int) = this(BigDecimal(value))
		lazy val toJSON = value
	}
	object Number extends JsonExtractor[BigDecimal,Number]
	
	class Arr(val value: Traversable[O]) extends O {
		lazy val toJSON = JSONArray(value map {_.toJSON} toList)
	}
	object Arr extends JsonExtractor[Traversable[O],Arr]
	
	class Object(val value: Map[String,O]) extends O {
		lazy val toJSON = JSONObject(value filter { _._2 != null } map {
			case (k,v) => (k, v.toJSON) 
		})
	}
	
	object ObjectProps {
		def unapplySeq(elems: Json.Object): Option[Seq[String]] = {
			Object.unapplySeq(elems).asInstanceOf[Option[Seq[String]]]
		}
	}
	
	object Object {
		def unapplySeq(elems: Json.Object): Option[Seq[O]] = Some(
			new collection.SeqProxy[O] {
				val self = elems.value.map { a => 
						val y = a._2 match {
							case s: Str => new Str(s.value)
							case n: Number => new Number(n.value)
							case a: Arr => new Arr(a.value)
							case o: Object => new Object(o.value)
						}
						y.props = elems.props
						y.m = elems.value
						y
					} toSeq
				override def apply(i: Int) = if (i >= size) mock else super.apply(i)
				override def lengthCompare(i: Int) = 0
				
				def mock = new O {
					def toJSON = throw new RuntimeException()
					m = elems.value
					props = elems.props
				}
			}
		)
	}

	def convertFromRaw: Any => Json.O = { raw =>
		val newContext = mutable.ArrayBuffer[String]()
		val o = raw match {
			case i: BigDecimal => new Number(i)
			case s: String => new Str(s)
			case a: List[_] => new Arr(a map convertFromRaw toSeq)
			case m: Map[String,_] => new Object(m map { case (a,b) => (a, convertFromRaw(b)) })
		}
		o.props = newContext
		o
	}
	
	implicit val StringIsSerializable: Serializable[String] = { case x => new Str(x) }
	implicit val IntIsSerializable: Serializable[Int] = { case x => new Number(x) }
	implicit val BigDecimalIsSerializable: Serializable[BigDecimal] = { case x => new Number(x) }
	
	implicit def TraversableIsSerializable[T: Serializable]: Serializable[Traversable[T]] = { case x => new Arr(x map {a => implicitly[Json.O](a)}) }
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
	
	implicit def TraversableIsDeserializable[T](implicit e: Deserializable[T]): Deserializable[Traversable[T]] = {
		case Arr(value) => value map { case x if e.isDefinedAt(x) => e(x) }
	}
	implicit def SeqIsDeserializable[T](implicit e: Deserializable[T]): Deserializable[Seq[T]] = {
		case Arr(value) => value map { case x if e.isDefinedAt(x) => e(x) } toSeq
	}
	implicit def SetIsDeserializable[T](implicit e: Deserializable[T]): Deserializable[Set[T]] = {
		case Arr(value) => value map { case x if e.isDefinedAt(x) => e(x) } toSet
	}
	
	implicit def OptionIsDeserializable[T](implicit i: Deserializable[T]): Deserializable[Option[T]] = {
		case x: O if x.m == null && i.isDefinedAt(x) => Some(i(x))
		case x: O if x.m != null && x.f != null && x.m.isDefinedAt(x.f) && OptionIsDeserializable(i).isDefinedAt(x.m(x.f)) => OptionIsDeserializable(i)(x.m(x.f))
		case _ => None
	}
	
	implicit val OptionalStringIsDeserializable: Deserializable[Option[String]] = OptionIsDeserializable
	implicit val OptionalIntIsDeserializable: Deserializable[Option[Int]] = OptionIsDeserializable
	implicit val OptionalBigDecimalIsDeserializable: Deserializable[Option[BigDecimal]] = OptionIsDeserializable
	
	implicit val TraversableStringIsDeserializable: Deserializable[Traversable[String]] = TraversableIsDeserializable
	implicit val TraversableIntIsDeserializable: Deserializable[Traversable[Int]] = TraversableIsDeserializable
	implicit val TraversableBigDecimalIsDeserializable: Deserializable[Traversable[BigDecimal]] = TraversableIsDeserializable
	
	implicit def string2pair(name: String) = new {
		def ->[B <% O](y: B): (String,Json.O) = (name,y)
	}
	
	private type T = (String,Json.O)
	implicit def Tuple1IsSerializable: Tuple1[T] => Json.O = t => new Json.Object(Map(t._1))
	implicit def Tuple2IsSerializable: Tuple2[T,T] => Json.O = t => new Json.Object(Map(t._1, t._2))
	implicit def Tuple3IsSerializable: Tuple3[T,T,T] => Json.O = t => new Json.Object(Map(t._1, t._2, t._3))
	implicit def Tuple4IsSerializable: Tuple4[T,T,T,T] => Json.O = t => new Json.Object(Map(t._1, t._2, t._3, t._4))
}