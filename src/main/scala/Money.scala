case class Money(val euros: Int, val cents: Int) {
	override def toString = euros + "€ and " + cents + " cents"
}