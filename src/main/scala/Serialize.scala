
import scala.util.parsing.json._
import Json._

object Serialize {
	
	// custom type
	implicit val money_s: Serializable[Money] = { case x =>
		x.euros + "." + x.cents + "€"
	}
	val regex = "([0-9]+)[.]([0-9]+)€".r
	implicit val money_d: Deserializable[Money] = {
		case Json.Str(regex(e, c)) => Money(euros = e.toInt, cents = c.toInt)
	}
	// due to (?) imperfect type inference, we must manually declare optional serializability
	implicit val money_od: Deserializable[Option[Money]] = OptionIsDeserializable
	
	// Fully statically type safe serialization
	// + compiler warns if the type is not serializable
	// + compiler warns when new parameters are added to the object (due to EmployeeDto(_,_,_,_))
	// + compiler warns when parameters are removed from the object 
	// + parameter order does not matter
	// - verbose syntax
	// - Json keys given manually
	implicit val employeedto_s: Serializable[EmployeeDto] = { case x@(EmployeeDto(_,_,_,_)) => (
		"name" -> x.name,
	  "age" -> x.age,
	  "salary" -> x.salary,
	  "bonus" -> x.bonus
  )}
						 		  
	// Almost statically type safe serialization
	// + compiler warns if the type is not serializable
	// - compiler does not warn when new parameters are added to the object
	// + compiler warns when parameters are removed from the object 
	// + parameter order does not matter
	// + clean syntax
	// - Json keys given manually
	implicit val departmentdto_s: Serializable[DepartmentDto] = { case x => (
		"name" -> x.name,
		"employees" -> x.employees
	)}
	
	// Almost statically type safe serialization 2
	// + compiler warns if the type is not serializable
	// + compiler warns when new parameters are added to the object
	// + compiler warns when parameters are removed from the object
	// - parameter order matters, i.e. switching 2 with the same type breaks json
	// + clean syntax
	// - Json keys given manually
	implicit val organizationdto_s: Serializable[OrganizationDto] = { case OrganizationDto(name, departments) => (
		"name" -> name,
		"departments" -> departments
	)}
	
	
	
	
	// fully statically type safe deserialization
	implicit val empoyeedto_d: Deserializable[EmployeeDto] = {
		case Json.Object(a@p("age"), 
										 n@p("name"),
										 s@p("salary"),
										 b@p("bonus")
				 						) => EmployeeDto(name = n, 
				 														 age = a, 
				 														 salary = s,
				 														 bonus = b)
	}
	
	// due to (?) imperfect type inference, we must manually declare Traversable serializability
	implicit val employeedto_ss: Serializable[Traversable[EmployeeDto]] = TraversableIsSerializable
	implicit val employeedto_sd: Deserializable[Seq[EmployeeDto]] = SeqIsDeserializable
	
	// almost statically type safe deserialization.
	// + Compiler warns if the amount of constructor method arguments is changed (due to 'pN' prefix where N is the amount of arguments)
	// - Compiler cannot warn if parameters are re-ordered in the object constructor.
	implicit val departmentdto_d: Deserializable[DepartmentDto] = p2 {
		case Json.ObjectProps("name", "employees") => DepartmentDto.apply _
	}
	
	implicit val departementdto_sd: Deserializable[Set[DepartmentDto]] = SetIsDeserializable
	
	implicit val organizationdto_d: Deserializable[OrganizationDto] = p2 {
		case Json.ObjectProps("name", "departments") => OrganizationDto.apply _
	}

	def main(args: Array[String]): Unit = {
		val employee = EmployeeDto("John Doe", Some(42), Money(3500, 50), Some(Money(2500, 20)))
		val employee2 = EmployeeDto("Jill Doe", None, Money(3499, 51), None)
		val department = DepartmentDto("IT", Seq(employee, employee2))
		val organization = OrganizationDto("Firma", Set(department))
		
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
		
		val organizationAsJson = Json serialize organization
		println("Organization serialized to JSON:")
		println(organizationAsJson)
		println
		
		val departmentEmployeesAsJson = Json serialize department.employees
		println("Department employees serialized to JSON:")
		println(departmentEmployeesAsJson)
		println
		
		val e = Json.deserialize[EmployeeDto](employeeAsJson)
		val e2 = Json.deserialize[EmployeeDto](employee2AsJson)
		val d = Json.deserialize[DepartmentDto](departmentAsJson)
		val o = Json.deserialize[OrganizationDto](organizationAsJson)
		
		println("Employees, department and organization deserialized from JSON:")
		println(e)
		println(e2)
		println(d)
		println(o)
		println
		
		println("Deserialization succeeding with missing optional properties")
		val withMissing = Json.deserialize[EmployeeDto]("""{"name": "Jack Doe", "salary": "14.03€"}""")
		println(withMissing)
		println
		
		println("Deserialization succeeding with additional properties")
		val withAdditional = Json.deserialize[EmployeeDto]("""{"name": "Jack Doe", "age": 42, "salary": "14.03€", "foo": "bar"}""")
		println(withAdditional)
		println
		
		println("Failing deserialization due to missing required property")
		val failed = Json.deserialize[EmployeeDto]("""{"name": "Jack Doe", "age": 42}""")
		println(failed)
	}

}