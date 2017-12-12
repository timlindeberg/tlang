var empty = ""
var s = "String"
var s2 = "string2"
println(s) // res: String
println(s2) // res: string2
println(new A()) // res: ObjectA


// Size / Empty / Non Empty
println(empty.Size()) // res: 0
println(empty.IsEmpty()) // res: true
println(empty.NonEmpty()) // res: false

println(s.Size()) // res: 6
println(s.IsEmpty()) // res: false
println(s.NonEmpty()) // res: true

// Iterator
for(val c in s)
	print(c + " ") // res: S t r i n g
println()

// To Numbers
val sixtyFour = "64"
val sixtyFourHex = "40"
val sixtyFourBinary = "01000000"
println(sixtyFour.ToInt() == 64) // res: true
println(sixtyFourHex.ToInt(16) == 64) // res: true
println(sixtyFourBinary.ToInt(2) == 64) // res: true

println(sixtyFour.ToLong() == 64) // res: true
println(sixtyFourHex.ToLong(16) == 64) // res: true
println(sixtyFourBinary.ToLong(2) == 64) // res: true

println(sixtyFour.ToFloat() == 64.0f) // res: true
println(sixtyFour.ToDouble() == 64.0) // res: true

// Equality
println(s == s) // res: true
println(s != s) // res: false
println(s == s2) // res: false
println(s != s2) // res: true

// Comparisons
println(s < s) // res: false
println(s <= s) // res: true
println(s > s) // res: false
println(s >= s) // res: true

println(s < s2) // res: true
println(s <= s2) // res: true
println(s > s2) // res: false
println(s >= s2) // res: false

// String addition
println(s + " " + s) // res: String String
println(s + 1) // res: String1
println(1 + s) // res: 1String
println(s + 1L) // res: String1
println(1L + s) // res: 1String
println(s + 1.0f) // res: String1.0
println(1.0f + s) // res: 1.0String
println(s + 1.0) // res: String1.0
println(1.0 + s) // res: 1.0String

// Multiplication
println(empty * 25) // res:
println(25 * empty) // res:
println(s * 3) // res: StringStringString
println(3 * s) // res: StringStringString
println(s + " " * 10 + s) // res: String          String

// Accessor
println(s[0]) // res: S
println(s[1]) // res: t
println(s[2]) // res: r
println(s[3]) // res: i
println(s[4]) // res: n
println(s[5]) // res: g

// Hash Code
println(#empty) // res: 0
println(#s) // res: -1808118735

// Capitalize
println("test".Capitalize()) // res: Test
println("tEST".Capitalize()) // res: TEST
println("TEST".Capitalize()) // res: TEST
println("TEST".Capitalize()) // res: TEST
println("Test".Capitalize()) // res: Test

// Slices
println("test"[:]) // res: test
println("test"[1:]) // res: est
println("test"[1:3]) // res: es
println("test"[::2]) // res: ts

// CharAt
println(s.CharAt(0)) // res: S
println(s.CharAt(1)) // res: t
println(s.CharAt(2)) // res: r
println(s.CharAt(3)) // res: i
println(s.CharAt(4)) // res: n
println(s.CharAt(5)) // res: g

// CodePointAt
println(s.CodePointAt(0)) // res: 83
println(s.CodePointAt(1)) // res: 116
println(s.CodePointAt(2)) // res: 114
println(s.CodePointAt(3)) // res: 105
println(s.CodePointAt(4)) // res: 110
println(s.CodePointAt(5)) // res: 103

// CodePointBefore
println(s.CodePointBefore(1)) // res: 83
println(s.CodePointBefore(2)) // res: 116
println(s.CodePointBefore(3)) // res: 114
println(s.CodePointBefore(4)) // res: 105
println(s.CodePointBefore(5)) // res: 110
println(s.CodePointBefore(6)) // res: 103

// CodePointCount
println(s.CodePointCount(0, 6)) // res: 6
println(s.CodePointCount(1, 3)) // res: 2

// Concat
println(s.Concat(s2)) // res: Stringstring2

// Contains
println(s.Contains("ring")) // res: true
println(s.Contains("S")) // res: true
println(s.Contains("String")) // res: true
println(s.Contains("lol")) // res: false

// EndsWith
println(s.EndsWith("ring")) // res: true
println(s.EndsWith("g")) // res: true
println(s.EndsWith("Strin")) // res: false
println(s.Contains("lol")) // res: false

// GetChars
var chars = [ 'a', 'b', 'c' ]
s.GetChars(1, 3, chars, 1)
for(val c in chars)
	print(c)
println() // res: atr

// IndexOf
val ss = s + s
val s2s2 = s2 + s2
println(ss.IndexOf('S')) // res: 0
println(ss.IndexOf('t')) // res: 1
println(ss.IndexOf('g')) // res: 5
println(ss.IndexOf('a')) // res: -1
println(ss.IndexOf("ng")) // res: 4

println(ss.LastIndexOf('S')) // res: 6
println(ss.LastIndexOf('t')) // res: 7
println(ss.LastIndexOf('g')) // res: 11
println(ss.LastIndexOf('a')) // res: -1
println(ss.LastIndexOf("ng")) // res: 10

// Matches
println(s.Matches("Str.*")) // res: true
println(s.Matches(".*")) // res: true
println(s.Matches("Str.*2")) // res: false
println(s2.Matches("str.*2")) // res: true

// OffsetByCodePoints
println(s.OffsetByCodePoints(0, 2)) // res: 2

// RegionMatches
println(s.RegionMatches(true, 0, s2, 0, 6)) // res: true
println(s.RegionMatches(false, 0, s2, 0, 6)) // res: false
println(s.RegionMatches(false, 1, s2, 0, 5)) // res: false
println(s.RegionMatches(false, 1, s2, 1, 5)) // res: true

// Replace
println(ss.Replace("Str", "A")) // res: AingAing
println(ss.ReplaceAll("S.*r", "A")) // res: Aing
println(ss.ReplaceAll("S.+?r", "A")) // res: AingAing
println(s2s2.Replace("2", "A")) // res: stringAstringA
println(s2s2.ReplaceAll("\\d", "A")) // res: stringAstringA

// Split
val sp = s2s2.Split("2")
println(sp[0]) // res: string
println(sp[1]) // res: string

// StartWith
println(s.StartsWith("Strin")) // res: true
println(s.StartsWith("S")) // res: true
println(s.StartsWith("tring")) // res: false
println(s.StartsWith("lol")) // res: false
println(s.StartsWith("rin", 2)) // res: true
println(s.StartsWith("r", 2)) // res: true
println(s.StartsWith("St", 2)) // res: false

// ToArray
chars = s.ToArray()
for(val c in chars)
	print(c + " ")
println() // res: S t r i n g

// ToLower/UpperCase
println(s.ToLowerCase()) // res: string
println(s.ToUpperCase()) // res: STRING
println(s2.ToLowerCase()) // res: string2
println(s2.ToUpperCase()) // res: STRING2

// Trim
val trim = "  \t   " + s + "      \t  "
println(trim.Trim()) //  res: String

// ValueOf
println(String.ValueOf(chars)) // res: String
println(String.ValueOf(chars, 1, 2)) // res: tr
println(String.ValueOf('c')) // res: c
println(String.ValueOf(true)) // res: true
println(String.ValueOf(false)) // res: false
println(String.ValueOf(1.5)) // res: 1.5
println(String.ValueOf(1.5f)) // res: 1.5
println(String.ValueOf(5)) // res: 5
println(String.ValueOf(5l)) // res: 5
println(String.ValueOf(5l)) // res: 5
println(String.ValueOf(new A())) // res: ObjectA

class A =
	Def toString() = "ObjectA"