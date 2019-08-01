trait MyAnnotation: Annotation =
	Def MyString(): String

@MyAnnotation(MyString = "", MyInt = 8) // res: T2012
class A

@MyAnnotation(MyString = 8) // res: T2000
class B
