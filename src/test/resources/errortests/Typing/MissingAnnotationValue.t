trait MyAnnotation: Annotation =
	Def MyString(): String
	Def MyInt(): Int
	Def MyFloat(): Float

@MyAnnotation // res: T2011
class A

@MyAnnotation(MyInt = 5, MyString = "ABC") // res: T2011
class B

@MyAnnotation(MyInt = 5, MyString = "ABC", MyFloat = 0.0f)
class C
