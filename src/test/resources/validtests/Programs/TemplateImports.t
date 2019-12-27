import T::std::Vector
import T::std::Tester

val v = new Vector<Int>()


v.Add(1)
println(v[0]) // res: 1

val v1 = new T::std::Vec3<Int>(0)
val v2 = new T::std::Vec3<Int>(1)

println(v1) // res: "(0, 0, 0)"
println(v2) // res: "(1, 1, 1)"

// TODO: Static template calls
//println(T::lang::Vec3<Int>.Dot(v1, v2))
//println(Tester<Int>.AssertEquals(1, 1))

for(val i in new A())
	println(i) // res: 0, 1, 2, 3, 4

class A : T::std::Iterable<Int> =

	Def Iterator() = new AIterator()

class AIterator : T::std::Iterator<Int> =

	var i = 0

	Def HasNext() = i < 5
	Def Next() = return i++
