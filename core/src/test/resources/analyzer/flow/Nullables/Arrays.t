var arr: A?[] = { new A(), null, GetA() }

arr[0].Test()
arr[1].Test() // res: F2001
arr[2].Test() // res: F2000

arr[1] = GetA()

if(arr[1] && arr[2] != null)
	arr[0].Test()
	arr[1].Test()
	arr[2].Test()

arr = new A?[15]
arr[0] = null

arr[0].Test() // res: F2001
arr[1].Test() // res: F2000
arr[2].Test() // res: F2000

if(arr[1] && arr[2])
	arr[0].Test() // res: F2001
	arr[1].Test()
	arr[2].Test()

var i = 5
var j = 5

if(arr[i])
	i = 0
	arr[i].Test() // res: F2001
	arr[j].Test()
	i = j
	arr[i].Test()


if(arr[i])
	val i = 0
	arr[j].Test()

arr = GetNullableArr()
i = 1
j = 1
if(arr[i + j])
	arr[i + j].Test()
	arr[2].Test()
	i--
	if(arr[i])
		arr[0].Test()

	++i
	if(arr[i])
		arr[1].Test()

var nullableNullableArr: A?[]?[] = { GetNullableArr(), null, GetNullableArr() }

if(nullableNullableArr[0] && nullableNullableArr[0][1])
	nullableNullableArr[0][1].Test()

nullableNullableArr = new A?[4]?[4]

if(nullableNullableArr[0] && nullableNullableArr[0][1])
	nullableNullableArr[0][1].Test()


arr = GetNullableArr()

if(arr[1] == null || arr[2] == null)
	return

arr[1].Test()
arr[2].Test()

/*------------------------------------------------------------------------*/

Def GetA(): A? = null
Def GetNullableArr(): A?[] = return { new A(), null, new A() }

class A =
	Def Test() = println("Test")

