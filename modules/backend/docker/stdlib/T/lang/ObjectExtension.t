package T::lang

extension java::lang::Object =

	Def #(obj: java::lang::Object) = obj.hashCode()
	Def ==(lhs: java::lang::Object, rhs: java::lang::Object) = lhs.equals(rhs)
	Def !=(lhs: java::lang::Object, rhs: java::lang::Object) = !(lhs == rhs)
