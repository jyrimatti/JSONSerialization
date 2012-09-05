
import scala.util.parsing.json._
import Json._

object Serialize {
	
	implicit val m2j: Serializable[Money] = { case x =>
		x.euros + "." + x.cents + "Û"
	}
	val regex = "([0-9]+)[.]([0-9]+)Û".r
	implicit val j2m: Deserializable[Money] = {
		case Json.Str(regex(e, c)) => Money(euros = e.toInt, cents = c.toInt)
	}
	
	implicit val e2j: Serializable[EmployeeDto] = { case x => (
				"name" -> x.name,
	 		  "age" -> x.age,
	 		  "salary" -> x.salary
  )}
	implicit val j2e: Deserializable[EmployeeDto] = {
		case Json.Object(a@O("age"), 
										 n@O("name"), 
										 s@O("salary")
				 						 ) => EmployeeDto(name = n, 
				 														  age = a, 
				 														  salary = s)
	}
						 		  
	implicit val d2j: Serializable[DepartmentDto] = { case x => (
				"name" -> x.name,
				"employees" -> x.employees
	)}
	
	implicit val se2j: Deserializable[Seq[EmployeeDto]] = SeqIsDeserializable
	implicit val j2d: Deserializable[DepartmentDto] = {
		case Json.Object(n@O("name"), 
										 e@O("employees")
										 ) => DepartmentDto(name = n, 
												 								employees = e)
	}

	def main(args: Array[String]): Unit = {
		val employee = EmployeeDto("John Doe", Some(42), Money(3500, 50))
		val employee2 = EmployeeDto("Jill Doe", None, Money(3499, 51))
		val department = DepartmentDto("IT", Seq(employee, employee2))
		
		val employeeAsJson = Json serialize employee
		println("Employee serialized to JSON:")
		println(employeeAsJson)
		println
		
		val employee2AsJson = Json serialize employee2
		println("Another employee serialized to JSON:")
		println(employee2AsJson)
		println
		
		val departmentAsJson = Json serialize department
		println("Department serialized to JSON:")
		println(departmentAsJson)
		println
		
		val e = Json.deserialize[EmployeeDto](employeeAsJson)
		val e2 = Json.deserialize[EmployeeDto](employee2AsJson)
		val d = Json.deserialize[DepartmentDto](departmentAsJson)
		
		println("Employees and department deserialized from JSON:")
		println(e)
		println(e2)
		println(d)
		println
		
		println("Deserialization succeeding with missing optional properties")
		val withMissing = Json.deserialize[EmployeeDto]("""{"name": "Jack Doe", "salary": "14.03Û"}""")
		println(withMissing)
		println
		
		println("Deserialization succeeding with additional properties")
		val withAdditional = Json.deserialize[EmployeeDto]("""{"name": "Jack Doe", "age": 42, "salary": "14.03Û", "foo": "bar"}""")
		println(withAdditional)
		println
		
		println("Failing deserialization due to missing required property")
		val failed = Json.deserialize[EmployeeDto]("""{"name": "Jack Doe", "age": 42}""")
		println(failed)
	}

}