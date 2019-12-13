Def NoReturn(): Int = // res: F2007
	println(5)

Def Return(): Int =
	return 5

Def NoReturnInUnitMethod(): Unit =
	println(5)

Def EarlyReturn(): Int =  // res: F2007
	if(true)
		return 5

	if(false)
		return 5

Def ReturnInIf(): Int =
	if(true)
		return 5
	else
		return 6

Def MissingReturnInThenBranch(): Int = // res: F2007
	if(true)
		println("ABC")
	else
		return 25

Def MissingReturnInElseBranch(): Int = // res: F2007
	if(true)
		return 25
	else
		println("ABC")

Def NestedMissingReturnInElseBranch(): Int = // res: F2007
	if(true)
		if(true)
			if(true)
				return 25
			else
				println("ABC")
		else
			return 25
	else
		return 25

Def NestedMissingReturnInThenBranch(): Int = // res: F2007
	if(true)
		if(true)
			if(true)
				println("ABC")
			else
				return 25
		else
			return 25
	else
		return 25

Def WithWhileLoop(): Int =   // res: F2007
	while(true)
		return 25

Def WithErrorMessage(): Int =
	error("ERROR")

Def WithErrorMessageInBranch(): Int =
	if(true)
		error("ERROR")
	else
		return 25

NoReturn()