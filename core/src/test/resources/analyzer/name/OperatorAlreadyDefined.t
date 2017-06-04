class A =

	Def +(lhs: A, rhs: A) = return 1
	Def +(lhs: A, rhs: A) = return 2 // res: N2009

	Def -(a: A) = return 1
	Def -(a: A) = return 2 // res: N2009
