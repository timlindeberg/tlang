package T::lang

class IntRef =

	var v: Int

	Def static ValueOf(v: Int) = new IntRef(v)
	Def Value() = v

	Def toString() = java::lang::String.valueOf(v)
	def new(v: Int) = (this.v = v)
