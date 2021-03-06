import T::std::Vector
import T::std::Tester

Construction()
Set()
IsEmpty()
Clear()
Iterator()
Add()
Remove()
RemoveIndex()
IndexOf()
Contains()
ToArray()
Equals()
MakeString()
Foreach()
Splice()
Assignment()

println("All tests succeeded.") // res: All tests succeeded.

Def Construction() =
	val t = new Tester<Int>()
	val v1: Vector<Int> = [ 1, 2, 3 ]
	t.AssertEquals(v1.Get(0), 1)
	t.AssertEquals(v1.Get(1), 2)
	t.AssertEquals(v1.Get(2), 3)
	t.AssertEquals(v1.Size(), 3)


	val v2: Vector<Int> = new Vector<Int>()
	t.AssertEquals(v2.Size(), 0)
	t.AssertTrue(v2.IsEmpty())

	val v3: Vector<Int> = new Vector<Int>(v1)
	t.AssertEquals(v3[0], 1)
	t.AssertEquals(v3[1], 2)
	t.AssertEquals(v3[2], 3)
	t.AssertEquals(v3.Size(), 3)

	val v4: Vector<Int> = new Vector<Int>(3, 1)
	t.AssertEquals(v4[0], 1)
	t.AssertEquals(v4[1], 1)
	t.AssertEquals(v4[2], 1)
	t.AssertEquals(v4.Size(), 3)

Def Set() =
	val t = new Tester<Int>()
	val v1: Vector<Int> = [ 1, 2, 3 ]

	v1.Set(1, 1)
	v1[2] = 1
	t.AssertEquals(v1[0], 1)
	t.AssertEquals(v1[1], 1)
	t.AssertEquals(v1[2], 1)
	t.AssertEquals(v1.Size(), 3)

Def IsEmpty() =
	val t = new Tester<Int>()
	val v1: Vector<Int> = [ 1, 2, 3 ]
	val v2: Vector<Int> = new Vector<Int>()

	t.AssertTrue(v1.NonEmpty())
	t.AssertFalse(v1.IsEmpty())

	t.AssertTrue(v2.IsEmpty())
	t.AssertFalse(v2.NonEmpty())

Def Clear() =
	val t = new Tester<Int>()
	val v1: Vector<Int> = [ 1, 2, 3 ]
	v1.Clear()

	t.AssertTrue(v1.IsEmpty())
	t.AssertEquals(v1.Size(), 0)

Def Iterator() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1, 2, 3 ]
	val it = v.Iterator()

	t.AssertTrue(it.HasNext())
	t.AssertEquals(it.Next(), 1)
	t.AssertTrue(it.HasNext())
	t.AssertEquals(it.Next(), 2)
	t.AssertTrue(it.HasNext())
	t.AssertEquals(it.Next(), 3)
	t.AssertFalse(it.HasNext())

	var i = 0
	for(val j in v)
		if(i == 0)
			t.AssertEquals(j, 1)
		else if(i == 1)
			t.AssertEquals(j, 2)
		else
			t.AssertEquals(j, 3)
		i++

Def Add() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1 ]
	v.AddFirst(0)
	t.AssertEquals(v[0], 0)
	t.AssertEquals(v[1], 1)
	t.AssertEquals(v.Size(), 2)

	v.Add(1, 2)
	t.AssertEquals(v[0], 0)
	t.AssertEquals(v[1], 2)
	t.AssertEquals(v[2], 1)
	t.AssertEquals(v.Size(), 3)

	v.Add(3)
	t.AssertEquals(v[0], 0)
	t.AssertEquals(v[1], 2)
	t.AssertEquals(v[2], 1)
	t.AssertEquals(v[3], 3)
	t.AssertEquals(v.Size(), 4)

	val v2: Vector<Int> = [ 4, 5, 6 ]
	v.AddAll(v2)
	t.AssertEquals(v[0], 0)
	t.AssertEquals(v[1], 2)
	t.AssertEquals(v[2], 1)
	t.AssertEquals(v[3], 3)
	t.AssertEquals(v[4], 4)
	t.AssertEquals(v[5], 5)
	t.AssertEquals(v[6], 6)
	t.AssertEquals(v.Size(), 7)

Def Remove() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1, 2, 3, 4, 5 ]
	val v2: Vector<Int> = [ 2, 3 ]

	t.AssertFalse(v.Remove(0))
	t.AssertFalse(v.Remove(6))

	t.AssertTrue(v.Remove(4))

	t.AssertEquals(v[0], 1)
	t.AssertEquals(v[1], 2)
	t.AssertEquals(v[2], 3)
	t.AssertEquals(v[3], 5)
	t.AssertEquals(v.Size(), 4)

	v.Remove(1)
	t.AssertEquals(v[0], 2)
	t.AssertEquals(v[1], 3)
	t.AssertEquals(v[2], 5)
	t.AssertEquals(v.Size(), 3)

	v.Remove(5)
	t.AssertEquals(v[0], 2)
	t.AssertEquals(v[1], 3)
	t.AssertEquals(v.Size(), 2)

	v.RemoveAll(v2)
	t.AssertEquals(v.Size(), 0)
	t.AssertTrue(v.IsEmpty())

Def RemoveIndex() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1, 2, 3, 4, 5 ]

	v.RemoveIndex(3)
	t.AssertEquals(v[0], 1)
	t.AssertEquals(v[1], 2)
	t.AssertEquals(v[2], 3)
	t.AssertEquals(v[3], 5)
	t.AssertEquals(v.Size(), 4)

	v.RemoveIndex(0)
	t.AssertEquals(v[0], 2)
	t.AssertEquals(v[1], 3)
	t.AssertEquals(v[2], 5)
	t.AssertEquals(v.Size(), 3)

	v.RemoveIndex(2)
	t.AssertEquals(v[0], 2)
	t.AssertEquals(v[1], 3)
	t.AssertEquals(v.Size(), 2)

Def IndexOf() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1, 2, 2, 4, 5, 1 ]

	t.AssertEquals(v.IndexOf(1), 0)
	t.AssertEquals(v.IndexOf(2), 1)
	t.AssertEquals(v.IndexOf(4), 3)
	t.AssertEquals(v.IndexOf(5), 4)


	t.AssertEquals(v.LastIndexOf(1), 5)
	t.AssertEquals(v.LastIndexOf(2), 2)
	t.AssertEquals(v.LastIndexOf(4), 3)
	t.AssertEquals(v.LastIndexOf(5), 4)

	t.AssertEquals(v.IndexOf(6), -1)
	t.AssertEquals(v.LastIndexOf(6), -1)

Def FirstLast() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1, 2, 2, 4, 5 ]

	t.AssertEquals(v.First(), 1)
	t.AssertEquals(v.Last(), 5)

Def Contains() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1, 2, 2, 4, 5, 1 ]
	val v2: Vector<Int> = [ 1, 4, 5 ]

	t.AssertTrue(v.Contains(1))
	t.AssertTrue(v.Contains(2))
	t.AssertTrue(v.Contains(4))
	t.AssertTrue(v.Contains(5))

	t.AssertFalse(v.Contains(6))

	t.AssertTrue(v.ContainsAll(v))
	t.AssertTrue(v.ContainsAll(v2))
	t.AssertFalse(v2.ContainsAll(v))

Def ToArray() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1, 2, 2, 4, 5, 1 ]
	val a: Int[] = v.ToArray()
	t.AssertEquals(a.Size(), 6)
	t.AssertEquals(a[0], 1)
	t.AssertEquals(a[1], 2)
	t.AssertEquals(a[2], 2)
	t.AssertEquals(a[3], 4)
	t.AssertEquals(a[4], 5)
	t.AssertEquals(a[5], 1)

Def MakeString() =
	val t = new Tester<String>()
	val v: Vector<Int> = [ 1, 2, 2, 4, 5, 1 ]
	t.AssertEquals(v.MakeString(" + "), "1 + 2 + 2 + 4 + 5 + 1")
	t.AssertEquals(v.toString(), "[ 1, 2, 2, 4, 5, 1 ]")

Def Equals() =
	val t = new Tester<Int>()

	val v1: Vector<Int> = [ 1, 2, 2, 4, 5, 1 ]
	val v2: Vector<Int> = [ 1, 2, 2, 4, 5, 1 ]
	val v3: Vector<Int> = [ 1, 2, 3, 4, 5, 1 ]
	val v4: Vector<Int> = [ 1, 4, 5 ]

	t.AssertTrue(v1 == v1)
	t.AssertTrue(v1 == v2)
	t.AssertTrue(v2 == v1)

	t.AssertFalse(v1 != v1)
	t.AssertFalse(v1 != v2)
	t.AssertFalse(v2 != v1)

	t.AssertFalse(v1 == v3)
	t.AssertFalse(v3 == v1)

	t.AssertFalse(v1 == v4)
	t.AssertFalse(v4 == v1)

	t.AssertTrue(v1 != v3)
	t.AssertTrue(v3 != v1)

	t.AssertTrue(v1 != v4)
	t.AssertTrue(v4 != v1)

Def Foreach() =
	val t = new Tester<Int>()
	val v: Vector<Int> = [ 1, 2, 3, 4, 5, 6 ]

	var i = 1
	for(val value in v)
		t.AssertEquals(i++, value)

Def Splice() =
	val t = new Tester<String>()
	val v: Vector<Int> = [ 1, 2, 3, 4, 5, 6 ]

	t.AssertEquals(v[:].toString(), "[ 1, 2, 3, 4, 5, 6 ]")
	t.AssertEquals(v[1:].toString(), "[ 2, 3, 4, 5, 6 ]")
	t.AssertEquals(v[4:].toString(), "[ 5, 6 ]")
	t.AssertEquals(v[:3].toString(), "[ 1, 2, 3 ]")
	t.AssertEquals(v[1:3].toString(), "[ 2, 3 ]")
	t.AssertEquals(v[::2].toString(), "[ 1, 3, 5 ]")
	t.AssertEquals(v[1::2].toString(), "[ 2, 4, 6 ]")

Def Assignment() =
	val t = new Tester<Long>()
	val v: Vector<Long> = [ 0L, 0L, 0L ]

	v[0] = v[1] = v[2] = 5
	t.AssertEquals(v[0], 5)
	t.AssertEquals(v[1], 5)
	t.AssertEquals(v[2], 5)

	v[0] *= 2
	v[0] -= 5
	v[0] += 2
	t.AssertEquals(v[0], 7)
