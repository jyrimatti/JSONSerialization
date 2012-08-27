case class EmployeeDto(
	name: String,
	age: Int,
	salary: Money
)

case class DepartmentDto(
		name: String,
		employees: Seq[EmployeeDto]
)
