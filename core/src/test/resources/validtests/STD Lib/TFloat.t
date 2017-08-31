println(Float.MaxExponent()) // res: 127
println(Float.MaxValue())    // res: 3.4028235E38

println(Float.MinExponent()) // res: -126
println(Float.MinValue())    // res: 1.4E-45
println(Float.MinNormal())   // res: 1.17549435E-38

println(Float.Size())   // res: 32
println(Float.Bytes())   // res: 4

println(Float.NaN())   // res: NaN
println(Float.PositiveInfinity())   // res: Infinity
println(Float.NegativeInfinity())   // res: -Infinity

println(0.12345f.ToIntBits()) // res: 1039979355
println(0.12345f.ToRawIntBits()) // res: 1039979355

println(0.12345f.IsInfinite()) // res: false
println(Float.PositiveInfinity().IsInfinite()) // res: true

println(0.12345f.IsNaN()) // res: false
println(Float.NaN().IsNaN()) // res: true

println(0.12345f.ToHexString()) // res: 0x1.f9a6b6p-4

println(0.12345f.toString()) // res: 0.12345