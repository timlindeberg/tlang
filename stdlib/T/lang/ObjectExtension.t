package T::lang

extension ObjectExtension : Object =

	Def #(obj: Object) = obj.hashCode()
	Def ==(lhs: Object, rhs: Object) = lhs.equals(rhs)
	Def !=(lhs: Object, rhs: Object) = !(lhs == rhs)
