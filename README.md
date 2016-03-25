# T-compiler
A Compiler project for the awesome new T-language. T is an object-oriented language compiled to the JVM and supports C++-like generics. The T compiler is written in Scala.

## Features
* Classes and inheritence.
* All basic data types supported by the JVM except for bytes and shorts.
* All the operators supported by Java as well as a hash operator (#). 
* Type inference for variables and methods.
* C++-style generics with templates. This enables for example lists of int where the integers does not have to be boxed.
* Operator overloading.
* Semicolons at the end of lines are not needed.
* Custom import system to handle templates.

## Planned features.
* New base class, currently all objects inherit from java.lang.object.
* First-class functions.
* Scala style traits with multiple inheritence.

## Code examples

### Test code for scoping rules
```
var i: Int = 1
println(i)
{
	var i: Int = 2
	println(i)
	i += 5
	println(i)
	if(i == 7) {
		var i: Int = 10
		println(i + 1)
	}
	println(i)
}
println(i)
for(var i: Int = 0; i < 5; i ++) print(i + " ")
println("")

println(i)

var j: Int = 1
for(var i: Int = 0, var j: Int = 0; i + j < 10; i++, j++) print((i + j) + " ")
println("")

println(i)
println(j)

Test(5)

Def Test(i: Int): Unit = {
	println(i)
	{
		var i: Int = 1
		println(i)
	}
	println(i)
	var i: Int = 1
	println(i)
}
```
Prints the results:
```
1
2
7
11
7
1
0 1 2 3 4
1
0 2 4 8
1
1
5
1
1
5
1

```


### Matrix implementation in T
```
class Matrix<T> {

    var rows: Int
    var columns: Int
    var data: MatrixRow<T>[]

    Def new(size: Int)               = init(size, size)
    Def new(rows: Int, columns: Int) = init(rows, columns)

    Def [](index: Int) = return data[index]
    Def []=(index: Int, row: T[]) = data[index] = new MatrixRow<T>(row)

    Def +(lhs: Matrix<T>, rhs: Matrix<T>) = {
        if(lhs.rows != rhs.rows || lhs.columns != rhs.columns)
            errorInvalidDimensions(lhs, rhs)

        var m = new Matrix<T>(lhs.columns, lhs.rows)
        for(var i = 0; i < m.columns; i++)
            for(var j = 0; j < m.rows; j++)
                m[i][j] = lhs[i][j] + rhs[i][j]

        return m
    }

    Def -(lhs: Matrix<T>, rhs: Matrix<T>) = {
        if(lhs.rows != rhs.rows || lhs.columns != rhs.columns)
            errorInvalidDimensions(lhs, rhs)

        var m = new Matrix<T>(lhs.columns, lhs.rows)
        for(var i = 0; i < m.columns; i++)
            for(var j = 0; j < m.rows; j++)
                m[i][j] = lhs[i][j] - rhs[i][j]

        return m
    }

    Def *(matrix: Matrix<T>, scalar: T) = return scalar * matrix
    Def *(scalar: T, matrix: Matrix<T>) = {
        var m = new Matrix<T>(matrix.columns, matrix.rows)
        for(var i = 0; i < m.columns; i++)
           for(var j = 0; j < m.rows; j++)
                m[i][j] = scalar * matrix[i][j]

        return m
    }

    Def *(lhs: Matrix<T>, rhs: Matrix<T>) = {
        if(lhs.columns != rhs.rows)
            errorInvalidDimensions(lhs, rhs)

        var m = new Matrix<T>(lhs.rows, lhs.columns)
        for(var i = 0; i < m.columns; i++)
            for(var j = 0; j < m.rows; j++)
                for(var k = 0; k < lhs.columns; k++)
                    m[i][j] += lhs[i][k] * rhs[k][j]

        return m
    }

    Def -(matrix: Matrix<T>) = return -1 * matrix

    Def #(matrix: Matrix<T>) = {
        var res: Int
        for(var i = 0; i < columns; i++)
           for(var j = 0; j < rows; j++)
               res = 31 * res + #matrix[i][j]

        return res
    }

    Def Transpose(): Matrix<T> = {
        var m = new Matrix<T>(columns, rows)
        for(var i = 0; i < columns; i++)
           for(var j = 0; j < rows; j++)
               m[i][j] = data[j][i]

        return m
    }

    Def Columns() = return columns
    Def Rows() = return rows

    Def toString() = {
        if(columns == 0 || rows == 0)
            return "[]"

        var s = ""
        for(var i = 0; i < rows; i++)
            s += data[i] + "\n"
        return s
    }

    def init(rows: Int, columns: Int) = {
        this.columns = columns
        this.rows = rows
        data = new MatrixRow<T>[rows]
        for(var i = 0; i < rows; i++)
            data[i] = new MatrixRow<T>(columns)
    }

    def errorInvalidDimensions(m1: Matrix<T>, m2: Matrix<T>) =
        error("Invalid dimensions for matrix operaton: (" + m1.rows + ", " + m1.columns + ") and (" + m2.rows + ", " + m2.columns + ").")
}

class MatrixRow<T> {

    var data: T[]

    Def new(w: Int) = data = new T[w]
    Def new(d: T[]) = data = d

    Def [](index: Int) = return data[index]

    Def []=(index: Int, value: T) = data[index] = value

    Def toString() = {
        if(data.length == 0)
            return "[]"

        var s = "[ "
        for(var i = 0; i < data.length; i++) s += data[i] + " "
        return s + "]"
    }

}
```

More code examples can be found in the folder 'src/kool/std'.