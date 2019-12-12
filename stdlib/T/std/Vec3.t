package T::std

import java::lang::Math

class Vec3<T> =

	Var X: T
	Var Y: T
	Var Z: T

	//------------------------------------------------------
	// Constructors
	//------------------------------------------------------

	Def new(x: Int, y: Int, z: Int) =
		X = x as T
		Y = y as T
		Z = z as T

	Def new(x: Long, y: Long, z: Long) =
		X = x as T
		Y = y as T
		Z = z as T

	Def new(x: Float, y: Float, z: Float) =
		X = x as T
		Y = y as T
		Z = z as T

	Def new(x: Double, y: Double, z: Double) =
		X = x as T
		Y = y as T
		Z = z as T

	Def implicit new(arr: T[]) =
		if(arr.Size() != 3)
			error("Cannot initialize Vec3 with array of dimension" + arr.Size())

		X = arr[0]
		Y = arr[1]
		Z = arr[2]

	Def new(value: T) = (X = Y = Z = value)

	//------------------------------------------------------
	// Operators
	//------------------------------------------------------

	Def +(lhs: Vec3<T>, rhs: Vec3<T>) = new Vec3<T>(lhs.X + rhs.X, lhs.Y + rhs.Y, lhs.Z + rhs.Z)

	Def -(lhs: Vec3<T>, rhs: Vec3<T>) = new Vec3<T>(lhs.X - rhs.X, lhs.Y - rhs.Y, lhs.Z - rhs.Z)

	Def *(lhs: Vec3<T>, rhs: Vec3<T>) = new Vec3<T>(lhs.X * rhs.X, lhs.Y * rhs.Y, lhs.Z * rhs.Z)

	Def *(lhs: Vec3<T>, rhs: Double) = new Vec3<T>(lhs.X * rhs, lhs.Y * rhs, lhs.Z * rhs)
	Def *(lhs: Vec3<T>, rhs: Float)  = new Vec3<T>(lhs.X * rhs, lhs.Y * rhs, lhs.Z * rhs)
	Def *(lhs: Vec3<T>, rhs: Int)    = new Vec3<T>(lhs.X * rhs, lhs.Y * rhs, lhs.Z * rhs)
	Def *(lhs: Vec3<T>, rhs: Long)   = new Vec3<T>(lhs.X * rhs, lhs.Y * rhs, lhs.Z * rhs)
	Def *(lhs: Double, rhs: Vec3<T>) = rhs * lhs
	Def *(lhs: Float, rhs: Vec3<T>)  = rhs * lhs
	Def *(lhs: Int, rhs: Vec3<T>)    = rhs * lhs
	Def *(lhs: Long, rhs: Vec3<T>)   = rhs * lhs

	Def /(lhs: Vec3<T>, rhs: Vec3<T>) = new Vec3<T>(lhs.X / rhs.X, lhs.Y / rhs.Y, lhs.Z / rhs.Z)

	Def /(lhs: Vec3<T>, rhs: Double) = new Vec3<T>(lhs.X / rhs, lhs.Y / rhs, lhs.Z / rhs)
	Def /(lhs: Vec3<T>, rhs: Float)  = new Vec3<T>(lhs.X / rhs, lhs.Y / rhs, lhs.Z / rhs)
	Def /(lhs: Vec3<T>, rhs: Int)    = new Vec3<T>(lhs.X / rhs, lhs.Y / rhs, lhs.Z / rhs)
	Def /(lhs: Vec3<T>, rhs: Long)   = new Vec3<T>(lhs.X / rhs, lhs.Y / rhs, lhs.Z / rhs)
	Def /(lhs: Double, rhs: Vec3<T>) = rhs / lhs
	Def /(lhs: Float, rhs: Vec3<T>)  = rhs / lhs
	Def /(lhs: Int, rhs: Vec3<T>)    = rhs / lhs
	Def /(lhs: Long, rhs: Vec3<T>)   = rhs / lhs

	Def ==(lhs: Vec3<T>, rhs: Vec3<T>) = lhs.X == rhs.X && lhs.Y == rhs.Y && lhs.Z == rhs.Z

	Def !=(lhs: Vec3<T>, rhs: Vec3<T>) = !(lhs == rhs)

	Def ^(lhs: Vec3<T>, rhs: Vec3<T>) = lhs.Cross(rhs)

	Def |(lhs: Vec3<T>, rhs: Vec3<T>) = lhs.Dot(rhs)

	Def -(vec: Vec3<T>) = new Vec3<T>(-vec.X, -vec.Y, -vec.Z)

	Def #(vec: Vec3<T>) =
		var hashCode = 1
		hashCode = 31 * hashCode + #vec.X
		hashCode = 31 * hashCode + #vec.Y
		hashCode = 31 * hashCode + #vec.Z
		hashCode

	//------------------------------------------------------
	// Public static methods
	//------------------------------------------------------

	Def static Angle(lhs: Vec3<T>, rhs: Vec3<T>) = Math.acos(Dot(lhs, rhs) / (lhs.Magnitude() * rhs.Magnitude()))

	Def static Distance(lhs: Vec3<T>, rhs: Vec3<T>) = (lhs - rhs).Magnitude()

	Def static Dot(lhs: Vec3<T>, rhs: Vec3<T>) = lhs.X * rhs.X + lhs.Y * rhs.Y + lhs.Z * rhs.Z

	Def static Cross(lhs: Vec3<T>, rhs: Vec3<T>) = new Vec3<T>(
	    lhs.Y * rhs.Z - lhs.Z * rhs.Y,
	    lhs.Z * rhs.X - lhs.X * rhs.Z,
	    lhs.X * rhs.Y - lhs.Y * rhs.X,
	)

	//------------------------------------------------------
	// Public methods
	//------------------------------------------------------

	Def Magnitude() = Math.sqrt(X * X + Y * Y + Z * Z)

	Def Normalized() = this / Magnitude()

	Def Angle(other: Vec3<T>) = Angle(this, other)

	Def Distance(other: Vec3<T>) = Distance(this, other)

	Def Dot(other: Vec3<T>) = Dot(this, other)

	Def Cross(other: Vec3<T>) = Cross(this, other)

	Def toString() = "(" + X + ", " + Y + ", " + Z + ")"
