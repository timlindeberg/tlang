println(new H("Construct").run()) // res: Run

class H =

	Def new(str: String) = println(str) // res: Construct

	Def run(): String = return "Run"
