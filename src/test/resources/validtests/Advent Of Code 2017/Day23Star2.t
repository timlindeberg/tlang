import java::lang::Math

val _input = `set b 79
set c b
jnz a 2
jnz 1 5
mul b 100
sub b -100000
set c b
sub c -17000
set f 1
set d 2
set e 2
set g d
mul g e
sub g b
jnz g 2
set f 0
sub e -1
set g e
sub g b
jnz g -8
sub d -1
set g d
sub g b
jnz g -13
jnz f 2
sub h -1
set g b
sub g c
jnz g 2
jnz 1 3
sub b -17
jnz 1 -23`

Def Decompiled() =
	val a = 1
	var b = 79
	var c = 79
	var d = 0
	var e = 0
	var f = 0
	var h = 0

	if(a != 0)
		b *= 100
		b += 100000
		c = b + 17000

	while(true)
		f = 1
		d = 2
		while(true)
			e = 2
			while(true)
				if(d * e == b)
					f = 0
				if(e == b) break
				e += 1
			if(d == b) break
			d += 1
		if(f == 0)
			h += 1

		if(b == c) break
		b += 17

Def IsPrime(n: Int) =
	if(n % 2 == 0) return false
	if(n % 3 == 0) return false

	var i = 5
	var w = 2

	while(i * i <= n)
		if(n % i == 0) return false
		i += w
		w = 6 - w

	return true

var count = 0
for(var b = 107900; b <= 107900 + 17000; b += 17)
	if(!IsPrime(b))
		count++
println(count) // res: 907
