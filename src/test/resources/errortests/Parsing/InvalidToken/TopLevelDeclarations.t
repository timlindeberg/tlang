trait "ABC" // res: P2000

Def ABC() = 25

class 25 = // res: P2000

	@Annotation
	Def Test() = "ABC"

// We get an unexpected dedeantation here so it's two errors
@Annotation // res: P2000
extension = // res: P2000
	Def Test() = "ABC"


// Here as well
Def 25(a: 52) = new  // res: P2000, P2000

annotation ABC: 25  // res: P2000
