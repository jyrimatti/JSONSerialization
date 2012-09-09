
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
	// + compiler warns if a type is not serializable
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
	// + compiler warns if a type is not serializable
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
	// + compiler warns if a type is not serializable
	// + compiler warns when new parameters are added to the object
	// + compiler warns when parameters are removed from the object
	// - parameter order matters, i.e. switching 2 with the same type breaks json
	// + clean syntax
	// - Json keys given manually
	implicit val divisiondto_s: Serializable[DivisionDto] = { case DivisionDto(name, departments, manager) => (
		"name" -> name,
		"departments" -> departments,
		"manager" -> manager
	)}
	
	// Almost statically type safe serialization by reflection
	// + compiler warns if a type is not serializable (better than regular reflection-based frameworks)
	// + compiler does not nag when new parameters are added to the object
	// + compiler does not nag when parameters are removed from the object
	// - serialized fields retrieved with getDeclaredFields and getMethod =( Can this be improved in Scala 2.10?
	// + clean syntax
	// + Json keys deduced by reflection
	implicit val organizationdto_s: Serializable[OrganizationDto] = OrganizationDto.apply _
	
	
	
	// Fully statically type safe deserialization
	// + compiler warns if a type is not deserializable
	// + compiler warns when new parameters are added to the object
	// + compiler warns when parameters are removed from the object 
	// + parameter order does not matter
	// - verbose syntax
	// - Json keys given manually
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
	implicit def employeedto_ss: Serializable[Traversable[EmployeeDto]] = TraversableIsSerializable
	implicit def employeedto_sd: Deserializable[Seq[EmployeeDto]] = SeqIsDeserializable
	implicit def departementdto_sd: Deserializable[Set[DepartmentDto]] = SetIsDeserializable
	
	// Almost statically type safe deserialization
	// + compiler warns if a type is not deserializable
	// + compiler warns when new parameters are added to the object (due to 'pN' prefix where N is the amount of arguments)
	// + compiler warns when parameters are removed from the object 
	// - compiler cannot warn if the order of the keys does not match that in the constructor
	// + clean syntax
	// - Json keys given manually
	implicit val departmentdto_d: Deserializable[DepartmentDto] = p2 {
		case Json.Keys("name", "employees") => DepartmentDto.apply
	}
	
	implicit val divisiondto_d: Deserializable[DivisionDto] = p3 {
		case Json.Keys("name", "departments", "manager") => DivisionDto.apply
	}
	
	// Almost statically type safe deserialization by reflection
	// + compiler warns if a type is not deserializable
	// +- compiler warns when new parameters are added to the object, but wouldn't have to (must use sN instead of overloaded s)
	// +- compiler warns when parameters are removed from the object, but wouldn't have to (must use sN instead of overloaded s)
	// - uses the given method as constructor but gets the parameters and their order from class fields =( Can this be improved in Scala 2.10?
	// + clean syntax
	// + Json keys deduced by reflection
	implicit val organizationdto_d: Deserializable[OrganizationDto] = p2 {
		case Json.Reflect() => OrganizationDto.apply
	}

	def main(args: Array[String]): Unit = {
		def test(msg: String, json: String) = {
			println(msg + " serialized to JSON:")
			println(json)
			println
			json
		}
		
		val employee = EmployeeDto("John Doe", Some(42), Money(3500, 50), Some(Money(2500, 20)))
		val employee2 = EmployeeDto("Jill Doe", None, Money(3499, 51), None)
		val department = DepartmentDto("IT", Seq(employee, employee2))
		val division = DivisionDto("Europe", Set(department), employee)
		val organization = OrganizationDto("Firma", Array(division))
		
		val employeeAsJson = test("Employee", Json serialize employee)
		val employee2AsJson = test("Another employee", Json serialize employee2)
		val departmentAsJson = test("Department", Json serialize department)
		val divisionAsJson = test("Division", Json serialize division)
		val organizationAsJson = test("Organization", Json serialize organization)
		test("Only employees of a department", Json serialize department.employees)
		
		val e = Json.deserialize[EmployeeDto](employeeAsJson)
		val e2 = Json.deserialize[EmployeeDto](employee2AsJson)
		val d = Json.deserialize[DepartmentDto](departmentAsJson)
		val di = Json.deserialize[DivisionDto](divisionAsJson)
		val o = Json.deserialize[OrganizationDto](organizationAsJson)
		
		println("Employees, department, division and organization deserialized from JSON:")
		Seq(e, e2, d, di, o, "") foreach println
		
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