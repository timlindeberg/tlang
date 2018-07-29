package T::lang

class DoubleRef =

	var v: Double

	Def static ValueOf(v: Double) = new DoubleRef(v)
	Def Value() = v

	Def toString() = java::lang::String.valueOf(v)
	def new(v: Double) = (this.v = v)
