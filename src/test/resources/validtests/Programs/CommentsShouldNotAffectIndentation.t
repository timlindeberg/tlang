Def MyFunction() =

	println(1) // res: 1

// Incorrectly indented comment
	println(2) // res: 2


class MyClass =

	Def F() =
		println(3) // res: 3

  // Incorrectly indented comment 1
 /*
  lel 2
*/
// Incorrectly indented comment 2


	/**/	println(4) // res: 4

	// Incorrectly indented comment
		println(5) // res: 5
/**/
	/**/
		/**/
		println(6) // res: 6

/**/		if (true)
// Incorrectly indented comment
/**/	/**/	/**/	/**/println(7) // res: 7



MyFunction()
new MyClass().F()
