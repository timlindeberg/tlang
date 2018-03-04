# Operators
`tlang` supports all the regular operators found in `Java` and a few more. Unless stated, the numerical
operators apply to all number types. The resulting type of a numerical operator depends on their operands:

- If any type is a `Double` the result is a `Double`.
- Otherwise, if any type is a `Float` the result is a `Float`.
- Otherwise, if any type is a `Long` the result is a `Long`. 
- Otherwise, the result is an `Int`. 

## Binary operators
 
### Plus
The plus operator, `+`, adds two numbers or two `String` s together.
```tlang
1 + 1 // 2
"ABC" + "DEF" // ABCDEF
``` 

### Minus
The minus operator, `-`, subtracts the right hand side from the left hand side.
```tlang
1 - 1 // 0
``` 

### Multiplication
The multiplication operator, `*`, multiplies two numbers.
```tlang
5 * 5 // 25
```
### Division
The division operator, `/`, divides the left hand side by the right hand side.
```tlang
25 / 5 // 5
```
### Modulo
The modulo operator, `%`, gives the remainder when dividing the the left hand side by right.
```tlang
11 % 3 // 2 
```
### Bitwise And
The bitwise and operator, `&`, compares each bit in the left hand side to each bit of the right hand side.
If both bits are `1` the corresponding bit is set to `1` otherwise it's set to `0`. 

Only applies to integral types.
```tlang
0b1111_1010 & 0b1100_0011 // 0b1100_0010
```
### Bitwise Or
The bitwise or operator, `|`, compares each bit in the left hand side to each bit of the right hand side.
If either of the bits are `1` the corresponding bit is set to `1` otherwise it's set to `0`. 

Only applies to integral types.
```tlang
0b1111_1010 | 0b1100_0011 // 0b1111_1011
```
### Bitwise Xor
The bitwise exclusive or operator, `^`, compares each bit in the left hand side to each bit of the right hand side.
If one bit is `1` and the other is `0` bit is set to `1` otherwise it's set to `0`. 

Only applies to integral types.
```tlang
0b1111_1010 ^ 0b1100_0011 // 0b0011_1001
```
### Left Shift
The left shift operator, `<<`, shifts the bits of the left hand operand by the right hand operand.

Only applies to integral types.
```tlang
0b0000_1111_1010 << 4 // 0b1111_1010
```
### Right Shift
The right shift operator, `>>`, shifts the bits of the left hand operand by the right hand operand.
The equivalent in Java would be `>>>`, the logical shift operator. This means that for a negative
number the sign will not be preserved.
 
Only applies to integral types.
```tlang
0b1111_1010 >> 4 // 0b0000_1111
```
### Less Than
The less than operator, `<`, compares the left hand operand by the right hand and returns true if
the left operand is smaller than the right.
```tlang
4 < 5 // true
5 = 5 // false
```
### Less Than Equals
The less than equals operator, `<=`, compares the left hand operand by the right hand and returns true if
the left operand is smaller than or equal to the right.
```tlang
4 <= 5 // true
5 <= 5 // true
```
### Greater Than
The greater than operator, `>`, compares the left hand operand by the right hand and returns true if
the left operand is smaller than or equal to the right.
```tlang
5 > 4 // true
5 > 5 // false
```
### Greater Than Equals
The greater than equals operator, `>=`, compares the left hand operand by the right hand and returns true if
the left operand is smaller than or equal to the right.
```tlang
5 >= 4 // true
5 >= 5 // true
```
### Equals
The equals operator, `==`, compares the left hand operand by the right hand and returns true if
they're equal.
```tlang
5 == 4 // false
5 == 5 // true
```
### Not Equals
The not equals operator, `!=`, compares the left hand operand by the right hand and returns true if
they're not equal.
```tlang
5 != 4 // true
5 != 5 // false
```
### And
The and operator, `&&`, accepts two boolean conditions and returns true if they're both true.
```tlang
false && false // false
false && true // false
true && false // false
true && true // true
```
### Or
The or operator, `||`, accepts two boolean conditions and returns true if they're both true.
```tlang
false || false // false
false || true // true
true || false // true
true || true // true
```

### Is
The is operator, `is`, is used to check the runtime type of an expression.
```tlang
5 is Int // true
new A() is A // true
"ABC" is Char // false
```

### As
The as operator, `as`, is used to cast a type to another. Numeric types can be cast to another
type to convert it from for example `Double` to `Int`. The `as` operator can also be used to
downcast a type and will throw a `CastException` when the cast is not possible.
```tlang
5.3 as Int // 5

12345 as Double // 12345.0 

class A : B
class B

val a: A = new A()
val b: B = a as B
val c: A = b as A

val x = new B()
val y = x as A // throws an exception
```

### Elvis
The elvis operator, `?:`, will return the right hand operand if the left operand is null. Otherwise,
the left hand operand is returned. The type of the left hand operand has to be nullable.
```tlang
var x: Int? = null
x ?: 1 // 1

x = 5
x ?: 1 // 5

5 ?: 1 // is an error since the left hand operand is not nullable
```

### Ternary
The ternary operator, `() ? () : ()`, evaluates the condition and returns the first argument if true
and the second one otherwise.
```tlang
true ? "ABC" : "123" // "ABC"
false ? "ABC" : "123" // "123"
```

### New
The new operator, `new`, constructs a new instance of a class.
```tlang
val x: A = new A()
```

## Unary operators

### Not
The not operator, `!`, inverses a boolean condition.
```tlang
!false // true
!true // false
```
### Hash
The hash operator, `#`, returns the hashcode of an object.
```tlang
#"ABC" // 64578
```
### Negation
The negation operator, `#`, returns the negative of the operand.
```tlang
-5 // -5
```
### Bitwise Not
The bitwise not operator, `~`, returns the operand with each bit switched.
```tlang
~0b1011_1111 // 0b0100_0000
```
### Extract Nullable
The extract nullable operator, `!!`, converts a nullable type into a non-nullable type. Use
this operator if you know that a variable is not `null`. If used on a variable containg `null`
a null pointer exception will be thrown.
```tlang
var a: Int? = 5
var b: Int = a!! // b == 5
a = null
b = a!! // throws NullPointerException
```
### Pre Increment
The pre increment operator, `++()`, modifies the underlying variable, field or array index, 
incrementing it by one and returns the incremented number.
```tlang
var a = 5;
val b = ++a;
// a == 6
// b == 6
```
### Pre Decrement
The pre decrement operator, `--()`, modifies the underlying variable, field or array index, 
decrementing it by one and returns the decremented number.
```tlang
var a = 5;
val b = --a;
// a == 4
// b == 4
```
### Post Increment
The post increment operator, `()++`, modifies the underlying variable, field or array index, 
incrementing it by one and returns the number as it was before it was incremented.
```tlang
var a = 5;
val b = a++;
// a == 6
// b == 5
```
### Post Decrement
The post decrement operator, `()--`, modifies the underlying variable, field or array index, 
decrementing it by one and returns the number as it was before it was decremented.
```tlang
var a = 5;
val b = a--;
// a == 4
// b == 5
```
## Indexing operators

### Indexing
The indexing operator, `[]`, can be used to index an array or a collection.
```tlang
val x = [1, 2, 3, 4, 5]
x[3] // 4
```
### Index assignment
The index assignment operator, `[]=`, can be used to assign a value to an index in an array or 
a collection.
```tlang
val x = [1, 2, 3, 4, 5]
x[3] = 5 // x = [1, 2, 3, 5, 5]
```
### Slice
The slice operator, `[:]`, returns a slice of the underlying array, string or collection. It accepts
three values, `start`, `end` and `step`. The result of applying the operator is a copy
of the underlying value containing entries from `start` up to but not including `end`. If `step`
is provided every `step`:th value will be included. All values can be skipped, in which case a 
default value is insterted:

- `start` is replaced by `0`
- `end` is replaced by `value.Size()`
- `step` is replaced by `1`

```tlang
val x = [1, 2, 3, 4, 5]

x[:]     // [1, 2, 3, 4, 5]
x[2:]    // [3, 4, 5]
x[:3]    // [1, 2, 3]
x[::2]   // [1, 3, 5]
x[::2]   // [1, 3, 5]
x[1:4:2] // [2, 4]
```

## Operator precedence
Operator precedence is shown below form highest to lowest. This applies for all types,
including overloaded operators.

| Precedence | Operators                                | Associativity |
| ---------- | ---------------------------------------- | ------------- |
| 17         | `[] [:] . ?. () as ()++ ()-- !!`         | Left to right |
| 16         | `! - ~ # ++() --() new`                  | Right to left |
| 15         | `* / %`                                  | Left to right |
| 14         | `+ -`                                    | Left to right |
| 13         | `<< >>`                                  | Left to right |
| 12         | `&`                                      | Left to right |
| 11         | `^`                                      | Left to right |
| 10         | `&#124;`                                 | Left to right |
| 9          | `< <= > >=`                              | Left to right |
| 8          | `as`                                     | Right to left |
| 7          | `is`                                     | Right to left |
| 6          | `== !=`                                  | Left to right |
| 5          | `&&`                                     | Left to right |
| 4          | `&#124;&#124;`                           | Left to right |
| 3          | `?:`                                     | Right to left |
| 2          | `() ? () : ()`                           | Right to left |
| 1          | `= += -= *= /= %= &= &#124;= ^= <<= >>=` | Right to left |
