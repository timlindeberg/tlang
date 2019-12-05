val start = 234208
val end = 765869

Def Matches(n: Int) =
	val s = n.toString()
	var hasMatchingDigits = false
	for(var i = 0; i < s.Size() - 1; i++)
		if (s[i + 1] < s[i])
			return false
		var matches = 0
		while (i < s.Size() - 1 && s[i] == s[i + 1])
			if (s[i + 1] < s[i])
				return false
			matches++
			i++
		if (i < s.Size() - 1 && s[i + 1] < s[i])
			return false
		if (matches == 1)
			hasMatchingDigits = true
	return hasMatchingDigits

var count = 0
for (var n = start; n <= end; n++)
	if(Matches(n)) count++

println(count) // res: 814
