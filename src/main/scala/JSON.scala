import scala.util.parsing.json._
import scala.math.ScalaNumber
import scala.collection.immutable.Set

import collection.mutable

object Json {
	abstract class O(var keysInOrder: mutable.Buffer[String] = mutable.ArrayBuffer(), 
									 var data: Map[String,O] = null, 
									 var key: String = null) {
		override def equals(other: Any) = other match {
	    case x: String => {
	    	key = x
	    	keysInOrder += x
	    	true 
	    }
	    case x: O => true
	  }
		def toJSON: Any
	}
	object p {
		def unapply(s: O): Option[String] = Some(s).asInstanceOf[Option[String]]
	}
	
	implicit def s1[T: Manifest,
	                E1: Serializable: Manifest](d: => (E1) => T): Serializable[T] = s2(d.asInstanceOf[(E1,String) => T])
	implicit def s2[T: Manifest,
	                E1: Serializable: Manifest,
	                E2: Serializable: Manifest](d: => (E1,E2) => T): Serializable[T] = s3(d.asInstanceOf[(E1,E2,String) => T])
	implicit def s3[T: Manifest,
	                E1: Serializable: Manifest,
	                E2: Serializable: Manifest,
	                E3: Serializable: Manifest](d: => (E1,E2,E3) => T): Serializable[T] = s4(d.asInstanceOf[(E1,E2,E3,String) => T])
	implicit def s4[T: Manifest,
	                E1: Serializable: Manifest,
	                E2: Serializable: Manifest,
	                E3: Serializable: Manifest,
	                E4: Serializable: Manifest](d: => (E1,E2,E3,E4) => T): Serializable[T] = {
		// Currently this uses field names from getDeclaredFields, which matches the constructor
		// only by accident...
		val keys = manifest[T].erasure.getDeclaredFields map {_.getName}
		// TODO: use scala 2.10 reflection to get constructor parameter names. Is this possible? With macros?
		{
			case a:T => new Object( (keys map { case k: String => (k, manifest[T].erasure.getMethod(k).invoke(a)) } zipWithIndex) map {
				case ((k,v),i) if i == 0 => (k,v.asInstanceOf[E1]: O)
				case ((k,v),i) if i == 1 => (k,v.asInstanceOf[E2]: O)
				case ((k,v),i) if i == 2 => (k,v.asInstanceOf[E3]: O)
				case ((k,v),i) if i == 3 => (k,v.asInstanceOf[E4]: O)
			} toMap)
		}
	}	
	
	implicit def p1[T: Manifest,
	                E1: Deserializable: Manifest](d: O => (E1) => T): Deserializable[T] = p2(d.asInstanceOf[O => (E1,String) => T])
	implicit def p2[T: Manifest,
	                E1: Deserializable: Manifest,
	                E2: Deserializable: Manifest](d: O => (E1,E2) => T): Deserializable[T] = p3(d.asInstanceOf[O => (E1,E2,String) => T])
	implicit def p3[T: Manifest,
	                E1: Deserializable: Manifest,
	                E2: Deserializable: Manifest,
	                E3: Deserializable: Manifest](d: O => (E1,E2,E3) => T): Deserializable[T] = p4(d.asInstanceOf[O => (E1,E2,E3,String) => T])
	implicit def p4[T: Manifest,
	                E1: Deserializable: Manifest,
	                E2: Deserializable: Manifest,
	                E3: Deserializable: Manifest,
	                E4: Deserializable: Manifest](d: O => (E1,E2,E3,E4) => T): Deserializable[T] = {
	  case o: Object => {
	  	val f: Any = d(o)
	  	val keys: Seq[String] = if (o.keysInOrder eq Reflect.Indicator) {
	  			// Currently this uses field names in getDeclaredFields order, which matches the constructor
	  			// only by accident...
	  			manifest[T].erasure.getDeclaredFields map {_.getName}
	  			// TODO: use scala 2.10 reflection to get constructor parameter names. Is this possible? With macros?
	  			/*import scala.reflect.runtime.universe.{== =>_,_}
	  			//val c = manifest[T].erasure.getConstructor(manifest[E1].erasure, manifest[E2].erasure)
	  			val methods = typeOf[T].members.collect {
	  				case m if m.isMethod => m.asMethodSymbol
	  			}
	  			val k = methods map { _.typeSignature } collect { 
	  				case MethodType(params, _) if (params map {_.typeSignature} toSeq).equals(Seq(typeOf[E1], typeOf[E2])) => params map {_.name.decoded}
	  			}
	  			k.head*/
	  		} else o.keysInOrder
			(keys map o.elements) match {
	  		case Seq() => println("bar"); null.asInstanceOf[T]
				case Seq(p1) => f.asInstanceOf[(E1)=>T](p1)
				case Seq(p1,p2) => f.asInstanceOf[(E1,E2)=>T](p1,p2)
				case Seq(p1,p2,p3) => f.asInstanceOf[(E1,E2,E3)=>T](p1,p2,p3)
				case Seq(p1,p2,p3,p4) => f.asInstanceOf[(E1,E2,E3,E4)=>T](p1,p2,p3,p4)
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
			case s: OT if s.data == null => Some(s.value)
			case o: O if o.data.isDefinedAt(o.key) => unapply(o.data(o.key))
			case _ => None
		}
	}
	
	class Str(val value: String) extends O {
		lazy val toJSON = value
	}
	
	class Number(val value: BigDecimal) extends O {
		lazy val toJSON = value
	}
	
	class Arr(val value: Traversable[O]) extends O {
		lazy val toJSON = JSONArray(value map {_.toJSON} toList)
	}
	
	class Object(val elements: Map[String,O]) extends O {
		lazy val toJSON = JSONObject(elements collect { case (k,v) if v != null => (k, v.toJSON) })
	}
	
	object Str extends JsonExtractor[String,Str]
	object Number extends JsonExtractor[BigDecimal,Number]
	object Arr extends JsonExtractor[Traversable[O],Arr]
	
	object Keys {
		def unapplySeq(elems: Object): Option[Seq[String]] = {
			Object.unapplySeq(elems).asInstanceOf[Option[Seq[String]]]
		}
	}
	
	object Reflect {
		val Indicator = mutable.ArrayBuffer[String]()
		def unapplySeq(o: Object): Option[Seq[Nothing]] = {
			o.keysInOrder = Indicator
			Some(Seq())
		}
		def toJSON = throw new RuntimeException
	}
	
	object Object {
		def unapplySeq(o: Object): Option[Seq[O]] = Some(
			new collection.SeqProxy[O] {
				val self = o.elements.values.map { v => 
						val y = v match {
							case s: Str => new Str(s.value)
							case n: Number => new Number(n.value)
							case a: Arr => new Arr(a.value)
							case o: Object => new Object(o.elements)
						}
						y.keysInOrder = o.keysInOrder
						y.data = o.elements
						y
					} toSeq
				override def apply(i: Int) = if (i >= size) mock else super.apply(i)
				override def lengthCompare(i: Int) = 0
				
				def mock = new O {
					def toJSON = throw new RuntimeException()
					data = o.elements
					keysInOrder = o.keysInOrder
				}
			}
		)
	}

	def convertFromRaw: Any => O = { raw =>
		val newContext = mutable.ArrayBuffer[String]()
		val o = raw match {
			case i: BigDecimal => new Number(i)
			case s: String => new Str(s)
			case a: List[_] => new Arr(a map convertFromRaw toSeq)
			case m: Map[String,_] => new Object(m map { case (a,b) => (a, convertFromRaw(b)) })
		}
		o.keysInOrder = newContext
		o
	}
	
	implicit val StringIsSerializable:     Serializable[String]     = { case x => new Str(x) }
	implicit val IntIsSerializable:        Serializable[Int]        = { case x => new Number(x) }
	implicit val BigDecimalIsSerializable: Serializable[BigDecimal] = { case x => new Number(x) }
	
	implicit def TraversableIsSerializable[T: Serializable]: Serializable[Traversable[T]] = { 
		case x => new Arr(x map {a => implicitly[O](a)})
	}
	implicit def ArrayIsSerializable[T: Serializable]: Serializable[Array[T]] = { 
		case x => new Arr(x map {a => implicitly[O](a)})
	}
	
	implicit def OptionalIsSerializable[T: Serializable]: Serializable[Option[T]] = {
		case Some(x) => x
		case None => null
	}
	
	implicit val StringIsDeserializable:     Deserializable[String]     = { case x@Json.Str(value) => value }
	implicit val IntIsDeserializable:        Deserializable[Int]        = { case x@Number(value) if value.isValidInt => value.intValue }
	implicit val BigDecimalIsDeserializable: Deserializable[BigDecimal] = { case x@Number(value) => value }
	
	implicit def TraversableIsDeserializable[T](implicit e: Deserializable[T]): Deserializable[Traversable[T]] = {
		case Arr(value) => value map { case x if e.isDefinedAt(x) => e(x) }
	}
	implicit def SeqIsDeserializable[T](implicit e: Deserializable[T]): Deserializable[Seq[T]] = {
		case Arr(value) => value map { case x if e.isDefinedAt(x) => e(x) } toSeq
	}
	implicit def ArrayIsDeserializable[T](implicit e: Deserializable[T], m: Manifest[T]): Deserializable[Array[T]] = {
		case Arr(value) => value map { case x if e.isDefinedAt(x) => e(x) } toArray
	}
	implicit def SetIsDeserializable[T](implicit e: Deserializable[T]): Deserializable[Set[T]] = {
		case Arr(value) => value map { case x if e.isDefinedAt(x) => e(x) } toSet
	}
	
	implicit def OptionIsDeserializable[T](implicit i: Deserializable[T]): Deserializable[Option[T]] = {
		case x: O if i.isDefinedAt(x) => Some(i(x))
		case _ => None
	}
	
	implicit val OptionalStringIsDeserializable:     Deserializable[Option[String]] =     OptionIsDeserializable
	implicit val OptionalIntIsDeserializable:        Deserializable[Option[Int]] =        OptionIsDeserializable
	implicit val OptionalBigDecimalIsDeserializable: Deserializable[Option[BigDecimal]] = OptionIsDeserializable
	
	implicit val TraversableStringIsDeserializable:     Deserializable[Traversable[String]] =     TraversableIsDeserializable
	implicit val TraversableIntIsDeserializable:        Deserializable[Traversable[Int]] =        TraversableIsDeserializable
	implicit val TraversableBigDecimalIsDeserializable: Deserializable[Traversable[BigDecimal]] = TraversableIsDeserializable
	
	implicit def string2pair(name: String) = new {
		def ->[B <% O](y: B): (String,O) = (name,y)
	}
	
	private type T = (String,O)
	implicit def tuple1toO: Tuple1[T] => O = t => new Object(Map(t._1))
	implicit def tuple2toO: Tuple2[T,T] => O = t => new Object(Map(t._1, t._2))
	implicit def tuple3toO: Tuple3[T,T,T] => O = t => new Object(Map(t._1, t._2, t._3))
	implicit def tuple4toO: Tuple4[T,T,T,T] => O = t => new Object(Map(t._1, t._2, t._3, t._4))
}