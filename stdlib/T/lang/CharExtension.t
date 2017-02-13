package T::lang

extension T::lang::Char {

    Def static MaxValue(): Char = java::lang::Character.MAX_VALUE
    Def static MinValue(): Char = java::lang::Character.MIN_VALUE
    Def static Size(): Int      = java::lang::Character.SIZE
    Def static Bytes(): Int     = java::lang::Character.BYTES

    Def static CharCount(codePoint: Int): Int                      = java::lang::Character.charCount(codePoint)
    Def static CodePointAt(a: Char[], index: Int): Int             = java::lang::Character.codePointAt(a, index)
    Def static CodePointAt(a: Char[], index: Int, limit: Int): Int = java::lang::Character.codePointAt(a, index, limit)
    Def static IsSurrogatePair(high: Char, low: Char): Bool        = java::lang::Character.isSurrogatePair(high, low)
    Def static ToChars(codePoint: Int): Char[]                     = java::lang::Character.toChars(codePoint)
    Def static ToCodePoint(high: Char, low: Char): Int             = java::lang::Character.toCodePoint(high, low)

    Def Digit(radix: Int): Int = java::lang::Character.digit(this, radix)
    Def Name(): String         = java::lang::Character.getName(this as Int)
    Def NumericValue(): Int    = java::lang::Character.getNumericValue(this)
    Def Type(): Int            = java::lang::Character.getType(this)
    Def HighSurrogate(): Char  = java::lang::Character.highSurrogate(this as Int)

    Def IsAlphabetic(): Bool             = java::lang::Character.isAlphabetic(this as Int)
    Def IsBmpCodePoint(): Bool           = java::lang::Character.isBmpCodePoint(this as Int)
    Def IsDefined(): Bool                = java::lang::Character.isDefined(this)
    Def IsDigit(): Bool                  = java::lang::Character.isDigit(this)
    Def IsHighSurrogate(): Bool          = java::lang::Character.isHighSurrogate(this)
    Def IsIdentifierIgnorable(): Bool    = java::lang::Character.isIdentifierIgnorable(this)
    Def IsIdeographic(): Bool            = java::lang::Character.isIdeographic(this)
    Def IsISOControl(): Bool             = java::lang::Character.isISOControl(this)
    Def IsJavaIdentifierPart(): Bool     = java::lang::Character.isJavaIdentifierPart(this)
    Def IsJavaIdentifierStart(): Bool    = java::lang::Character.isJavaIdentifierStart(this)
    Def IsLetter(): Bool                 = java::lang::Character.isLetter(this)
    Def IsLower(): Bool                  = java::lang::Character.isLowerCase(this)
    Def IsLowSurrogate(): Bool           = java::lang::Character.isLowSurrogate(this)
    Def IsMirrored(): Bool               = java::lang::Character.isMirrored(this)
    Def IsSpaceChar(): Bool              = java::lang::Character.isSpaceChar(this)
    Def IsSupplementaryCodePoint(): Bool = java::lang::Character.isSupplementaryCodePoint(this)
    Def IsSurrogate(): Bool              = java::lang::Character.isSurrogate(this)
    Def IsTitleCase(): Bool              = java::lang::Character.isTitleCase(this)
    Def IsUnicodeIdentifierPart(): Bool  = java::lang::Character.isUnicodeIdentifierPart(this)
    Def IsUnicodeIdentifierStart(): Bool = java::lang::Character.isUnicodeIdentifierStart(this)
    Def IsUpper(): Bool                  = java::lang::Character.isUpperCase(this)
    Def IsValidCodePoint(): Bool         = java::lang::Character.isValidCodePoint(this)
    Def IsWhitespace(): Bool             = java::lang::Character.isWhitespace(this)

    Def LowSurrogate(): Char  = java::lang::Character.lowSurrogate(this)
    Def ReverseBytes(): Char  = java::lang::Character.reverseBytes(this)

    Def ToLower(): Char      = java::lang::Character.toLowerCase(this)
    Def ToUpper(): Char      = java::lang::Character.toUpperCase(this)
    Def ToTitleCase(): Char  = java::lang::Character.toTitleCase(this)

    Def toString(): String = java::lang::Character.toString(this)

}