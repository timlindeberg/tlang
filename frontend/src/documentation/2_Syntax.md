# Significant white spaces
`tlang` is a significant white space language, meaning that whitespaces is relevant to how the grammar 
is parsed similar to languages like `Haskell` and `python`.

There are two types of significant whitespaces in `tlang`, newlines and tabs. A newline is used to end
a statement:

```tlang
var a = new A()
println(a) 
```

Statements can also be ended with a semicolon but it's not generally recommended:

```tlang
Def F() = 
	println("ABC"); println("DEF"); println("GHI")
```

## Tabs for indentation
In `tlang` tabs are used for indentation and spaces are used for alignment. This allows each programmer to choose an indentation 
width of their liking while still maintaining correct alignment. Since the rules are clear there
can't be any conflict of whether spaces or tabs should be used. Tabs are used to declare
a block and spaces have no syntactic meaning.

```tlang
Def F() =
	var x = 1 + 1
	x += 5
	for(var i = 0; i < 5; i++)
		x += 1
		x++
	println(x)
```

What follows after the function declaration is a block, as signified by the increase in indentation through
tab characters (seen above as arrows). The for loop on line `4` also has a block, signified by an indentation
level of 2.

Spaces are only used for alignment, hence the following code is invalid:

```tlang
Def F() =
    var x = 1 + 1
    x += 5
```

Note the lack of tabs after the function declaration. This is interpreted by the `tlang` compiler as:

```tlang
Def F() = var x = 1 + 1

x += 5
```

since there is no block following the function declaration. A valid use of spaces is to align things for
better readability:

```tlang
Def F() = 
	var a = [
	    1,
	    2,
	    3,
	]
```

Note that all lines in the block above only have an indentation level of one, the numbers in the array
literal are then indented using spaces.
