
import JSON._

object Serialize {
	
	type =>:[A,B] = PartialFunction[A,B]
	
	implicit val MoneyIsSerializable: Money => JSON.Obj = m => JSON.Str(m.euros + "." + m.cents + "Û")
	
	implicit val EmployeeDtoIsSerializable: EmployeeDto =>: JSON.Obj = {
		case EmployeeDto(name, age, salary) => JSON.Map(
				"name" -> name,
	 		  "age" -> age,
	 		  "salary" -> salary
	  )
	}
						 		  
	implicit val DepartmentDtoIsSerializable: DepartmentDto =>: JSON.Obj = {
		case DepartmentDto(name, employees) => JSON.Map(
				"name" -> name,
				"employees" -> employees
		)
	}
		

	def main(args: Array[String]): Unit = {
		
		
		val employee = EmployeeDto("John Doe", 42, Money(3500, 50))
		println(JSON serialize employee)
		
		val employee2 = EmployeeDto("Jill Doe", 69, Money(3499, 51))
		val department = DepartmentDto("IT", Seq(employee, employee2))
		
		println(JSON serialize department)
	}

}