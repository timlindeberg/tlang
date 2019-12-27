package T::lang

extension BoolExtension: Bool =

	Def toString(): String = java::lang::Boolean.toString(this)

	Def static Parse(s: String): Bool = java::lang::Boolean.parseBoolean(s)
