package tlang.repl

import com.googlecode.lanterna.input.{KeyStroke, KeyType}

class ReplSpec extends ReplIntegrationTestSpec {

  it should "execute simple commands" in {
    testTerminal
      .executeCommand("4 + 4")
      .stopWhen { box => box.contains("Result") }
      .shouldDisplay(
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;32mResult\u001b[0m                                    │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[35m4 \u001b[37m+ \u001b[35m4\u001b[0m                                                                        │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[34mval \u001b[36mres0\u001b[37m: \u001b[36mInt \u001b[37m= \u001b[35m8\u001b[0m                                                            │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
  }

  it should "use existing variables" in {
    testTerminal
      .executeCommand("5 * res0")
      .stopWhen { box => box.contains("Result") }
      .shouldDisplay(
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;32mResult\u001b[0m                                    │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[35m5 \u001b[37m* \u001b[36mres0\u001b[0m                                                                     │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[34mval \u001b[36mres1\u001b[37m: \u001b[36mInt \u001b[37m= \u001b[35m40\u001b[0m                                                           │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
  }

  it should "define and use classes" in {
    testTerminal
      .executeCommand(
        s"""|class A =
            |\tDef Times(t: Int) =
            |\tfor(var i = 0; i < t; i++)
            |\tprintln("A" + i)
            |""".stripMargin,
        KeyType.Backspace,
        KeyType.Backspace,
        KeyType.Backspace,
        "new A().Times(10)"
      )
      .stopWhen { box => box.contains("Result") }
      .shouldDisplay(
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;32mResult\u001b[0m                                    │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[34mclass \u001b[36mA \u001b[37m=\u001b[0m                                                                    │
            |│    \u001b[34mDef \u001b[36mTimes\u001b[37m(\u001b[36mt\u001b[37m: \u001b[36mInt\u001b[37m) =\u001b[0m                                                       │
            |│       \u001b[34mfor\u001b[37m(\u001b[34mvar \u001b[36mi \u001b[37m= \u001b[35m0\u001b[37m; \u001b[36mi \u001b[37m< \u001b[36mt\u001b[37m; \u001b[36mi\u001b[37m++)\u001b[0m                                             │
            |│          \u001b[34mprintln\u001b[37m(\u001b[33m"A" \u001b[37m+ \u001b[36mi\u001b[37m)\u001b[0m                                                    │
            |│ \u001b[34mnew \u001b[36mA\u001b[37m().\u001b[36mTimes\u001b[37m(\u001b[35m10\u001b[37m)\u001b[0m                                                            │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[1mDefined \u001b[1;34mclass \u001b[1;36mA\u001b[0m                                                              │
            |│ \u001b[36mA0\u001b[0m                                                                           │
            |│ \u001b[36mA1\u001b[0m                                                                           │
            |│ \u001b[36mA2\u001b[0m                                                                           │
            |│ \u001b[36mA3\u001b[0m                                                                           │
            |│ \u001b[1;32m... 6 more\u001b[0m                                                                   │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
  }

  it should "define and use functions" in {
    testTerminal
      .executeCommand(
        s"""|def Fun(a: Int, b: String) = b * a
            |
            |Fun(5, "ABC")
            |""".stripMargin
      )
      .stopWhen { box => box.contains("Result") }
      .shouldDisplay(
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;32mResult\u001b[0m                                    │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[34mdef \u001b[36mFun\u001b[37m(\u001b[36ma\u001b[37m: \u001b[36mInt\u001b[37m, \u001b[36mb\u001b[37m: \u001b[36mString\u001b[37m) = \u001b[36mb \u001b[37m* \u001b[36ma\u001b[0m                                           │
            |│                                                                              │
            |│ \u001b[36mFun\u001b[37m(\u001b[35m5\u001b[37m, \u001b[33m"ABC"\u001b[37m)\u001b[0m                                                                │
            |│                                                                              │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[1mDefined \u001b[1;34mmethod \u001b[1;36mFun\u001b[1;37m(\u001b[1;36mInt\u001b[1;37m, \u001b[1;36mString\u001b[1;37m)\u001b[0m                                              │
            |│ \u001b[34mval \u001b[36mres2\u001b[37m: \u001b[36mString \u001b[37m= \u001b[36mABCABCABCABCABC\u001b[0m                                           │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
  }

  it should "handle imports" in {
    testTerminal
      .executeCommand(
        s"""|import java::util::Date
            |
            |new Date(0)
            |""".stripMargin
      )
      .stopWhen { box => box.contains("Result") }
      .shouldDisplay(
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;32mResult\u001b[0m                                    │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[34mimport \u001b[36mjava\u001b[37m::\u001b[36mutil\u001b[37m::\u001b[36mDate\u001b[0m                                                      │
            |│                                                                              │
            |│ \u001b[34mnew \u001b[36mDate\u001b[37m(\u001b[35m0\u001b[37m)\u001b[0m                                                                  │
            |│                                                                              │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[1mImported \u001b[1;36mjava\u001b[1;37m::\u001b[1;36mutil\u001b[1;37m::\u001b[1;36mDate\u001b[0m                                                    │
            |│ \u001b[34mval \u001b[36mres3\u001b[37m: \u001b[36mDate \u001b[37m= \u001b[36mThu Jan \u001b[35m01 01\u001b[37m:\u001b[35m00\u001b[37m:\u001b[35m00 \u001b[36mCET \u001b[35m1970\u001b[0m                                │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
  }


  it should "show compilation errors" in {
    testTerminal
      .executeCommand(""""ABC" + a + res0 + b + "ABC"""")
      .stopWhen { box => box.contains("Error") }
      .shouldDisplay(
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;31mError\u001b[0m                                     │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[33m"ABC" \u001b[37m+ \u001b[1;4;31ma\u001b[0m\u001b[36m \u001b[37m+ \u001b[36mres0 \u001b[37m+ \u001b[1;4;31mb\u001b[0m\u001b[36m \u001b[37m+ \u001b[33m"ABC"\u001b[0m                                                 │
            |├──────┬───────────────────────────────────────────────────────────────────────┤
            |│ \u001b[35m1\u001b[0m:\u001b[35m9\u001b[0m  │ \u001b[1;31mError N2010\u001b[0m \u001b[1mCould not resolve symbol \u001b[1;35ma\u001b[0m\u001b[1m.\u001b[0m                               │
            |│ \u001b[35m1\u001b[0m:\u001b[35m20\u001b[0m │ \u001b[1;31mError N2010\u001b[0m \u001b[1mCould not resolve symbol \u001b[1;35mb\u001b[0m\u001b[1m.\u001b[0m                               │
            |└──────┴───────────────────────────────────────────────────────────────────────┘
            |""".stripMargin)
  }

  it should "show stack traces on errors" in {
    testTerminal
      .executeCommand("""error("ABC")""")
      .stopWhen { box => box.contains("Error") }
      .shouldDisplay(
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;31mError\u001b[0m                                     │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│ \u001b[34merror\u001b[37m(\u001b[33m"ABC"\u001b[37m)\u001b[0m                                                                 │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[1;31mjava\u001b[1;37m.\u001b[1;31mlang\u001b[1;37m.\u001b[1;31mRuntimeException\u001b[1;37m:\u001b[0m\u001b[37m \u001b[33mABC\u001b[0m                                              │
            |│    \u001b[1mat\u001b[0m\u001b[36m \u001b[1;31mReplExecution\u001b[1;37m.\u001b[1;34mmain\u001b[1;37m(\u001b[0m\u001b[36mUnknown Source\u001b[37m)\u001b[0m                                     │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
  }

  it should "exit when pressing CTRL+C" in {
    testTerminal
      .executeCommand(new KeyStroke('c', true, false))
      .stopWhen { box => box.contains("Thanks") }
      .shouldDisplay(
        s"""|╒══════════════════════════════════════════════════════════════════════════════╕
            |│                                    \u001b[1;35mInput\u001b[0m                                     │
            |╞══════════════════════════════════════════════════════════════════════════════╡
            |│                                                                              │
            |├──────────────────────────────────────────────────────────────────────────────┤
            |│ \u001b[1mThanks for using the \u001b[1;32mT-REPL\u001b[0m\u001b[1m!\u001b[0m                                                 │
            |└──────────────────────────────────────────────────────────────────────────────┘
            |""".stripMargin
      )
  }

}
