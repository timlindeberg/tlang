package T::lang

import java::lang::StringBuilder
import java::util::Locale

import T::std::Iterable
import T::std::Iterator

extension StringExtension: String =

	Def Iterator(): Iterator<Char> = new StringIterator(this)

	Def Size() = length()
	Def IsEmpty() = Size() == 0
	Def NonEmpty() = !IsEmpty()

	//----------------------------------------------------
	// Conversion methods
	//----------------------------------------------------

	Def ToInt()            = java::lang::Integer.parseInt(this)
	Def ToInt(radix: Int)  = java::lang::Integer.parseInt(this, radix)

	Def ToLong()           = java::lang::Long.parseLong(this)
	Def ToLong(radix: Int) = java::lang::Long.parseLong(this, radix)

	Def ToFloat()          = java::lang::Float.parseFloat(this)
	Def ToDouble()         = java::lang::Double.parseDouble(this)

	//----------------------------------------------------
	// Operators
	//----------------------------------------------------

	Def <(lhs: String, rhs: String)  = lhs.compareTo(rhs) < 0
	Def <=(lhs: String, rhs: String) = lhs.compareTo(rhs) <= 0
	Def >(lhs: String, rhs: String)  = lhs.compareTo(rhs) > 0
	Def >=(lhs: String, rhs: String) = lhs.compareTo(rhs) >= 0

	Def +(lhs: String, rhs: Object?): String =
		val b = String.valueOf(rhs ?: "null")
		lhs.concat(b)

	Def +(lhs: Object?, rhs: String): String =
		val a = String.valueOf(lhs ?: "null")
		a.concat(rhs)

	Def *(times: Long, str: String) = str * times
	Def *(str: String, times: Long) =
		val sb = new StringBuilder()
		for(var i = 0; i < times; i++)
			sb.append(str)
		sb.toString()

	Def *(times: Int, str: String) = str * (times as Long)
	Def *(str: String, times: Int) = str * (times as Long)

	Def [](index: Int) = charAt(index)

	Def [:](start: Int?, end: Int?, step: Int?): String =
		val s  = start ?: 0
		val e  = end   ?: Size()
		val st = step  ?: 1
		if(st == 1)
			return substring(s, e)

		val capacity = (e - s + st - 1) / st
		val chars = new Char[capacity]
		for(var i = s; i < e; i += st)
			chars[(i - s) / st] = charAt(i)

		new String(chars)

	Def Capitalize() =
		if(length() == 0 || this[0].IsUpper())
			return this
		this[0].ToUpper() + substring(1)

	Def Lines(): String[] = Split("\r?\n")

	//----------------------------------------------------------------
	// Wrappers for java.lang.String methods to enable capital letters
	//----------------------------------------------------------------

	Def CharAt(index: Int) = charAt(index)
	Def CodePointAt(index: Int) = codePointAt(index)
	Def CodePointBefore(index: Int) = codePointBefore(index)
	Def CodePointCount(beginIndex: Int, endIndex: Int) = codePointCount(beginIndex, endIndex)
	Def Concat(str: String) = concat(str)
	Def Contains(str: String) = contains(str)
	Def EndsWith(str: String) = endsWith(str)

	// TODO: Define format methods when var args is implemented
	//Def Format(l: Locale, format: String, args: Object*)
	//Def Format(format: String, args: Object*)

	// TODO: Define getByte methods when bytes are implemented
	//Def GetBytes() = getBytes()
	//Def GetBytes(charset: Charset) = getBytes(charset)
	//Def GetBytes(charsetName: String) = getBytes(charsetName)
	Def GetChars(srcBegin: Int, srcEnd: Int, dst: Char[], dstBegin: Int) = getChars(srcBegin, srcEnd, dst, dstBegin)

	Def IndexOf(c: Char)                     = indexOf(c as Int)
	Def IndexOf(c: Char, fromIndex: Int)     = indexOf(c as Int, fromIndex)
	Def IndexOf(str: String)                 = indexOf(str)
	Def IndexOf(str: String, fromIndex: Int) = indexOf(str, fromIndex)

	Def LastIndexOf(c: Char)                     = lastIndexOf(c as Int)
	Def LastIndexOf(c: Char, fromIndex: Int)     = lastIndexOf(c as Int, fromIndex)
	Def LastIndexOf(str: String)                 = lastIndexOf(str)
	Def LastIndexOf(str: String, fromIndex: Int) = lastIndexOf(str, fromIndex)

	Def Matches(regex: String) = matches(regex)

	Def OffsetByCodePoints(index: Int, codePointOffset: Int) = offsetByCodePoints(index, codePointOffset)
	Def RegionMatches(ignoreCase: Bool, toffset: Int, other: String, ooffset: Int, len: Int) = regionMatches(ignoreCase, toffset, other, ooffset, len)
	Def RegionMatches(toffset: Int, other: String, ooffset: Int, len: Int) = regionMatches(toffset, other, ooffset, len)
	Def Replace(oldChar: Char, newChar: Char) = replace(oldChar, newChar)
	Def Replace(target: String, replacement: String)     = replace(target, replacement)
	Def ReplaceAll(regex: String, replacement: String)   = replaceAll(regex, replacement)
	Def ReplaceFirst(regex: String, replacement: String) = replaceFirst(regex, replacement)

	Def Split(regex: String)             = split(regex)
	Def Split(regex: String, limit: Int) = split(regex, limit)

	Def Substring(beginIndex: Int)                = substring(beginIndex)
	Def Substring(beginIndex: Int, endIndex: Int) = substring(beginIndex, endIndex)

	Def StartsWith(prefix: String)               = startsWith(prefix)
	Def StartsWith(prefix: String, toffset: Int) = startsWith(prefix, toffset)

	Def ToArray()                   = toCharArray()
	Def ToLowerCase()               = toLowerCase()
	Def ToLowerCase(locale: Locale) = toLowerCase(locale)

	Def ToUpperCase()               = toUpperCase()
	Def ToUpperCase(locale: Locale) = toUpperCase(locale)

	Def Trim() = trim()

	Def static ValueOf(data: Char[], offset: Int, count: Int) = String.valueOf(data, offset, count)
	Def static ValueOf(data: Char[]) = String.valueOf(data)
	Def static ValueOf(c: Char)      = String.valueOf(c)
	Def static ValueOf(b: Bool)      = String.valueOf(b)
	Def static ValueOf(d: Double)    = String.valueOf(d)
	Def static ValueOf(f: Float)     = String.valueOf(f)
	Def static ValueOf(i: Int)       = String.valueOf(i)
	Def static ValueOf(l: Long)      = String.valueOf(l)
	Def static ValueOf(o: Object)    = String.valueOf(o)

class StringIterator: Iterator<Char> =

	var s: String
	var index = 0

	Def new(str: String) = (this.s = str)

	Def HasNext() = index < s.length()
	Def Next() = s.charAt(index++)
