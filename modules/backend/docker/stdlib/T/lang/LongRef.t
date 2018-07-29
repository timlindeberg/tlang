package T::lang

class LongRef =

	var v: Long

	Def static ValueOf(v: Long) = new LongRef(v)
	Def Value() = v

	Def toString() = java::lang::String.valueOf(v)
	def new(v: Long) = (this.v = v)
