case class EmployeeDto(
	name: String,
	age: Option[Int],
	salary: Money,
	bonus: Option[Money]
)

case class DepartmentDto(
	name: String,
	employees: Seq[EmployeeDto]
)

case class OrganizationDto(
	name: String,
	departments: Set[DepartmentDto]
)