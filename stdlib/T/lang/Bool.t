package T::lang

class Bool =

	Def new()        = ;
	Def new(v: Bool) = ;

	// Logical operators

	Def &(a: Bool, b: Bool) : Bool = ;
	Def |(a: Bool, b: Bool) : Bool = ;
	Def ^(a: Bool, b: Bool) : Bool = ;

	// Equals operators

	Def ==(a: Bool, b: Bool): Bool = ;
	Def !=(a: Bool, b: Bool): Bool = ;

	// Unary operators

	Def #(a: Bool) : Int = ;
