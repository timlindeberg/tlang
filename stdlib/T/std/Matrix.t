package T::std

class Matrix<T> =

	var rows: Int
	var columns: Int
	var data: MatrixRow<T>[]

	Def new(size: Int)               = init(size, size)
	Def new(rows: Int, columns: Int) = init(rows, columns)
	Def implicit new(arrays: T[][]) =
		if(arrays.Size() == 0 || arrays[0].Size() == 0)
			init(0, 0)
			return

		init(arrays.Size(), arrays[0].Size())
		for(var i = 0; i < columns; i++)
			for(var j = 0; j < rows; j++)
				this[i][j] = arrays[i][j]

	Def [](index: Int) = data[index]
	Def []=(index: Int, row: T[]) = (data[index] = new MatrixRow<T>(row))

	Def +(lhs: Matrix<T>, rhs: Matrix<T>) =
		if(lhs.rows != rhs.rows || lhs.columns != rhs.columns)
			errorInvalidDimensions(lhs, rhs)

		val m = new Matrix<T>(lhs.columns, lhs.rows)
		for(var i = 0; i < m.columns; i++)
			for(var j = 0; j < m.rows; j++)
				m[i][j] = lhs[i][j] + rhs[i][j]

		m

	Def -(lhs: Matrix<T>, rhs: Matrix<T>) =
		if(lhs.rows != rhs.rows || lhs.columns != rhs.columns)
			errorInvalidDimensions(lhs, rhs)

		val m = new Matrix<T>(lhs.columns, lhs.rows)
		for(var i = 0; i < m.columns; i++)
			for(var j = 0; j < m.rows; j++)
				m[i][j] = lhs[i][j] - rhs[i][j]

		m

	Def *(matrix: Matrix<T>, scalar: T) = scalar * matrix
	Def *(scalar: T, matrix: Matrix<T>) =
		val m = new Matrix<T>(matrix.columns, matrix.rows)
		for(var i = 0; i < m.columns; i++)
			for(var j = 0; j < m.rows; j++)
				m[i][j] = scalar * matrix[i][j]

		m

	Def *(lhs: Matrix<T>, rhs: Matrix<T>) =
		if(lhs.columns != rhs.rows)
			errorInvalidDimensions(lhs, rhs)

		val m = new Matrix<T>(lhs.rows, lhs.columns)
		for(var i = 0; i < m.columns; i++)
			for(var j = 0; j < m.rows; j++)
				for(var k = 0; k < lhs.columns; k++)
					m[i][j] += lhs[i][k] * rhs[k][j]

		m

	Def -(matrix: Matrix<T>) = -1 * matrix

	Def #(matrix: Matrix<T>) =
		var res: Int
		for(var i = 0; i < matrix.columns; i++)
			for(var j = 0; j < matrix.rows; j++)
				res = 31 * res + #matrix[i][j]

		res

	Def Transpose(): Matrix<T> =
		val m = new Matrix<T>(columns, rows)
		for(var i = 0; i < columns; i++)
			for(var j = 0; j < rows; j++)
				m[i][j] = data[j][i]

		m

	Def Columns() = columns
	Def Rows() = rows

	Def toString() =
		if(columns == 0 || rows == 0)
			return "[]"

		var s = ""
		for(var i = 0; i < rows; i++)
			s += data[i] + "\n"
		s

	def init(rows: Int, columns: Int) =
		this.columns = columns
		this.rows = rows
		data = new MatrixRow<T>[rows]
		for(var i = 0; i < rows; i++)
			data[i] = new MatrixRow<T>(columns)

	def static errorInvalidDimensions(m1: Matrix<T>, m2: Matrix<T>) =
		error("Invalid dimensions for matrix operation: (" + m1.rows + ", " + m1.columns + ") and (" + m2.rows + ", " + m2.columns + ").")

class MatrixRow<T> =

	var data: T[]

	Def new(w: Int) = (data = new T[w])
	Def new(d: T[]) = (data = d)

	Def [](index: Int) = data[index]

	Def []=(index: Int, value: T) = data[index] = value

	Def toString() =
		if(data.Size() == 0)
			return "[]"

		var s = "[ "
		for(var i = 0; i < data.Size(); i++)
			s += data[i] + " "
		s + "]"
