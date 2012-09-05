case class EmployeeDto(
	name: String,
	age: Option[Int],
	salary: Money
)

case class DepartmentDto(
		name: String,
		employees: Seq[EmployeeDto]
)
