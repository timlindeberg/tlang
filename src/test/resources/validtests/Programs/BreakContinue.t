for(var i = 0; i < 5; i++)
	println(i) // res: 0, 1, 2
	if(i >= 2)
		break

for(var i = 0; i < 5; i++)
	if(i > 3)
		continue
	for(var i = 0; i < 5; i++)
		if(i > 3)
			continue
		println(i) // res: 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3, 0, 1, 2, 3
	println(i) // res: 0, 1, 2, 3

var i = 0
while(i < 5)
	println(i) // res: 0, 1, 2
	if(i >= 2)
		break
	i++

i = 0
while(i < 6)
	i++
	if(i > 4)
		continue

	println(i) // res: 1, 2, 3, 4

for(var i = 0; i < 3; i++)
	if(i == 0)
		println("continue") // res: continue
		continue

	for(var j = 0; j < 2 && i < 2; j++)
		if(j < 1)
			println("continue") // res: continue
			continue

	while(i < 2)
		println("break") // res: break
		break
