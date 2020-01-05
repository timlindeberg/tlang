class Test =

	Def implicit new(i: Int) = ;
	Def implicit test() = return 1 // res: P2001
	Def implicit +(t1: Test, t2: Test) = return 1 // res: P2001
