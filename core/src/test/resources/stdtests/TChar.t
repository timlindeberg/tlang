println('a'.Size())      // res: 16
println('a'.Bytes())     // res: 2

println(Char.CharCount(1234)) // res: 1
println(Char.CodePointAt({'a', 'b', 'c'}, 1)) // res: 98
println(Char.CodePointAt({'a', 'b', 'c'}, 1, 3)) // res: 98
// println(Char.ToChars(1234)
println(Char.ToCodePoint('a', 'b')) // res: -56514462

println('5'.Digit(10)) // res: 5
println('5'.Name()) // res: DIGIT FIVE
println('5'.NumericValue()) // res: 5
println('5'.Type()) // res: 9


println('a'.IsAlphabetic())    // res: true
println('a'.IsBmpCodePoint())  // res: true
println('a'.IsDefined())       // res: true
println('5'.IsDigit())         // res: true
println('5'.IsHighSurrogate()) // res: false
println('5'.IsHighSurrogate()) // res: false
println('5'.IsIdentifierIgnorable()) // res: false
println('5'.IsIdeographic()) // res: false
println('5'.IsISOControl()) // res: false
println('5'.IsJavaIdentifierPart()) // res: true
println('5'.IsJavaIdentifierStart()) // res: false
println('5'.IsLetter()) // res: false
println('5'.IsLower()) // res: false
println('5'.IsLowSurrogate()) // res: false
println('5'.IsMirrored()) // res: false
println('5'.IsSpaceChar()) // res: false
println('5'.IsSupplementaryCodePoint()) // res: false
println('5'.IsSurrogate()) // res: false
println('5'.IsTitleCase()) // res: false
println('5'.IsUnicodeIdentifierPart()) // res: true
println('5'.IsUnicodeIdentifierStart()) // res: false
println('5'.IsUpper()) // res: false
println('5'.IsValidCodePoint()) // res: true
println('5'.IsWhitespace()) // res: false

println('5'.LowSurrogate()) // res: ?

println('A'.ToLower()) // res: a
println('a'.ToUpper()) // res: A
println('a'.ToTitleCase()) // res: A