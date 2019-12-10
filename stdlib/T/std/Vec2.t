package T::std

import java::lang::Math

class Vec2<T> =

	Var X: T
	Var Y: T

	//------------------------------------------------------
	// Constructors
	//------------------------------------------------------

	Def new(x: Int, y: Int) =
		X = x as T
		Y = y as T

	Def new(x: Long, y: Long) =
		X = x as T
		Y = y as T

	Def new(x: Float, y: Float) =
		X = x as T
		Y = y as T

	Def new(x: Double, y: Double) =
		X = x as T
		Y = y as T

	Def implicit new(arr: T[]) =
		if(arr.Size() != 2)
			error("Cannot initialize Vec2 with array of dimension" + arr.Size())

		X = arr[0]
		Y = arr[1]

	Def new(value: T) = (X = Y = value)

	//------------------------------------------------------
	// Operators
	//------------------------------------------------------

	Def +(lhs: Vec2<T>, rhs: Vec2<T>) = new Vec2<T>(lhs.X + rhs.X, lhs.Y + rhs.Y)

	Def -(lhs: Vec2<T>, rhs: Vec2<T>) = new Vec2<T>(lhs.X - rhs.X, lhs.Y - rhs.Y)

	Def *(lhs: Vec2<T>, rhs: Vec2<T>) = new Vec2<T>(lhs.X * rhs.X, lhs.Y * rhs.Y)

	Def *(lhs: Vec2<T>, rhs: Double) = new Vec2<T>(lhs.X * rhs, lhs.Y * rhs)
	Def *(lhs: Vec2<T>, rhs: Float)  = new Vec2<T>(lhs.X * rhs, lhs.Y * rhs)
	Def *(lhs: Vec2<T>, rhs: Int)    = new Vec2<T>(lhs.X * rhs, lhs.Y * rhs)
	Def *(lhs: Vec2<T>, rhs: Long)   = new Vec2<T>(lhs.X * rhs, lhs.Y * rhs)
	Def *(lhs: Double, rhs: Vec2<T>) = rhs * lhs
	Def *(lhs: Float, rhs: Vec2<T>)  = rhs * lhs
	Def *(lhs: Int, rhs: Vec2<T>)    = rhs * lhs
	Def *(lhs: Long, rhs: Vec2<T>)   = rhs * lhs

	Def /(lhs: Vec2<T>, rhs: Vec2<T>) = new Vec2<T>(lhs.X / rhs.X, lhs.Y / rhs.Y)

	Def /(lhs: Vec2<T>, rhs: Double) = new Vec2<T>(lhs.X / rhs, lhs.Y / rhs)
	Def /(lhs: Vec2<T>, rhs: Float)  = new Vec2<T>(lhs.X / rhs, lhs.Y / rhs)
	Def /(lhs: Vec2<T>, rhs: Int)    = new Vec2<T>(lhs.X / rhs, lhs.Y / rhs)
	Def /(lhs: Vec2<T>, rhs: Long)   = new Vec2<T>(lhs.X / rhs, lhs.Y / rhs)
	Def /(lhs: Double, rhs: Vec2<T>) = rhs / lhs
	Def /(lhs: Float, rhs: Vec2<T>)  = rhs / lhs
	Def /(lhs: Int, rhs: Vec2<T>)    = rhs / lhs
	Def /(lhs: Long, rhs: Vec2<T>)   = rhs / lhs

	Def ==(lhs: Vec2<T>, rhs: Vec2<T>) = lhs.X == rhs.X && lhs.Y == rhs.Y

	Def !=(lhs: Vec2<T>, rhs: Vec2<T>) = !(lhs == rhs)

	Def |(lhs: Vec2<T>, rhs: Vec2<T>) = lhs.Dot(rhs)

	Def -(vec: Vec2<T>) = new Vec2<T>(-vec.X, -vec.Y)

	Def #(vec: Vec2<T>) = 31 * #vec.X ^ 31 * #vec.Y

	//------------------------------------------------------
	// Public static methods
	//------------------------------------------------------

	Def static Angle(lhs: Vec2<T>, rhs: Vec2<T>) = Math.acos(Dot(lhs, rhs) / (lhs.Magnitude() * rhs.Magnitude()))

	Def static Distance(lhs: Vec2<T>, rhs: Vec2<T>) = (lhs - rhs).Magnitude()

	Def static Dot(lhs: Vec2<T>, rhs: Vec2<T>) = lhs.X * rhs.X + lhs.Y * rhs.Y

	//------------------------------------------------------
	// Public methods
	//------------------------------------------------------

	Def Magnitude() = Math.sqrt(X * X + Y * Y)

	Def Normalized() = this / Magnitude()

	Def Angle(other: Vec2<T>) = Angle(this, other)

	Def Distance(other: Vec2<T>) = Distance(this, other)

	Def Dot(other: Vec2<T>) = Dot(this, other)

	Def toString() = "(" + X + ", " + Y + ")"
