object JSON {
	trait Obj
	
	def serialize: Obj => String = _.toString
	
	case class Str(value: String) extends Obj {
		override def toString = '"' + value + '"'
	}
	case class Integ(value: Int) extends Obj {
		override def toString = value toString
	}
	case class Arr(value: Seq[Obj]) extends Obj {
		override def toString = value mkString ("[", ", ", "]")
	}
	case class Map(value: (String,Obj)*) extends Obj {
		override def toString = value map {case (l,r) => l + ": " + r} mkString ("{\n", ",\n", "\n}")
	}
	
	implicit val StringIsSerializable: String => JSON.Obj = Str(_)
	implicit val IntIsSerializable: Int => JSON.Obj = Integ(_)
	implicit def SeqIsSerializable[T <% Obj](s: Seq[T]): JSON.Obj = Arr(s map {a => implicitly[Obj](a)})
}