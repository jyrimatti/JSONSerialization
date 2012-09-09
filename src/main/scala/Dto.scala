case class EmployeeDto(
	name: String,
	age: Option[Int],			// optional built-in type
	salary: Money,				// custom type
	bonus: Option[Money]	// optional custom type
)

// Classes don't need to be case classes
class DepartmentDto(
	val name: String,
	val employees: Seq[EmployeeDto]	 // Seq
) {
	override def toString = "DepartmentDto(" + name + "," + employees + ")"
}
object DepartmentDto {
	def apply(name: String, e: Seq[EmployeeDto]) = new DepartmentDto(name, e)
}

case class DivisionDto(
	name: String,
	departments: Set[DepartmentDto],	// Set
	manager: EmployeeDto
)

case class OrganizationDto(
	name: String,
	divisions: Array[DivisionDto]			// Array
)