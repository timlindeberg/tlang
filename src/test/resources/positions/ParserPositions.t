class Class<T> : T, B, C =

	var a: Int = 0
	Var b = 1
	val protected c: Long
	Val static d: Float = "hej"

	def implicit new(a: Double, b: String[]) = ;

	Def +(a: Class<T>, b: Class<T>): Class<T> = ;

	def static Func2(): Unit = ;

	Def Func(d: Bool?, e: Char) =
		if(!a < #b && -a <= ~b && a!! > ++b && a++ >= --b)
			println(a + b)
		else
			print(a - b)

		while(a-- == b || a != b)
			error(a * b)

		for(var i = 0; i < 5; i++)
			continue

		for(var x in b)
			break

		Func2(a / b)
		var a = [
		    1 % 1l,
		    1.0 & 1.3e-5f,
		    10 | 5,
		    5.6 ^ 'a',
		    "hej" << true,
		    false >> null,
		]


		A?.Func(a[b])
		this.Func(a[:])
		a.Func(a[  1  :  2  :  3  ])
		super.Func(a[:2])
		a[b] = c as A

		a = a[1:]
		a.b = a[ 1 : 2 : ]
		c = new A[5]
		d = (a is String) ? b : (c ?: 5)
		d = new String(5, 7)
		return d


@Annotation @AnnotationB(a = "abc", b = 1)
trait B =
	Def Func(@Annotation @AnnotationB(a = "abc", b = 1) a: Int)

extension A: B
