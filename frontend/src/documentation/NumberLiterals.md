# Number Literals

`tlang` supports the usual numeric types used on the JVM such as `Int`, `Long`, `Float` and `Double`. Currently `Byte`
and `Short` is not supported but will be implemented in future versions. A number literal without decimal points ending 
with `L` or `l` is of type `Long`, otherwise it's an `Int`. If a number contains a decimal point and ends with `F` or 
`f` it's a `Float`, otherwise it's a `Double`.

Examples:

```tlang
1          // Int
1L         // Long
5l         // Long
1.0f       // Float
1.0        // Double
1.0e-12    // Double
128.256E8F // Float
```

Number literals can also include underscores to easier visualize large numbers:

```tlang
1_000_000
5_5_5_5L
1_000_000.1234F
0xAA_BB_CCL
0b0000_1111_0101_1111
```

## Hexademical literals

`tlang` supports hexadecimal literals by prefexing the number with `0x`:

```tlang
0xFF           // 255
0x1234567ABCDE // 255
```

## Binary literals

Binary literals are also supported by prefexing the number with `0b`:

```tlang
0b10       // 3
0b11111111 // 255
```
