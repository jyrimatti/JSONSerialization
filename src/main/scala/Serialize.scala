
import scala.util.parsing.json._
import Json._

object Serialize {
	
	implicit val MoneyIsSerializable: Serializable[Money] = { 
		case Money(euros, cents) => euros + "." + cents + "Û"
	}
	val regex = "([0-9]+)[.]([0-9]+)Û".r
	implicit val MoneyIsDeserializable: Deserializable[Money] = {
		case Json.Str(regex(euros, cents)) => Money(euros.toInt, cents.toInt)
	}
	
	implicit val EmployeeDtoIsSerializable: Serializable[EmployeeDto] = {
		case EmployeeDto(name, age, salary) => (
				"name" -> name,
	 		  "age" -> age,
	 		  "salary" -> salary
	  )
	}
	implicit val EmployeeDtoIsDeserializable: Deserializable[EmployeeDto] = _ match {
		case Json.Object(("name", name), ("age", age), ("salary", salary)) => EmployeeDto(name, age, salary)
	}
						 		  
	implicit val DepartmentDtoIsSerializable: Serializable[DepartmentDto] = {
		case DepartmentDto(name, employees) => (
				"name" -> name,
				"employees" -> employees
		)
	}
	implicit val DepartmentDtoIsDeserializable: Deserializable[DepartmentDto] = {
		case Json.Object(("name", name), ("employees", employees)) => DepartmentDto(name, SeqIsDeserializable(employees))
	}

	def main(args: Array[String]): Unit = {
		
		val employee = EmployeeDto("John Doe", 42, Money(3500, 50))
		val eJson = Json serialize employee
		println("Employee serialized to JSON:")
		println(eJson)
		println
		
		val employee2 = EmployeeDto("Jill Doe", 69, Money(3499, 51))
		val department = DepartmentDto("IT", Seq(employee, employee2))
		val dJson = Json serialize department
		println("Department serialized to JSON:")
		println(dJson)
		println
		
		val e: EmployeeDto = Json deserialize eJson get
		val d: DepartmentDto = Json deserialize dJson get
		
		println("Employee and department deserialized from JSON:")
		println(e)
		println(d)
		println
	}

}