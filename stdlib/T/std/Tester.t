package T::std

class Tester<T> {

    Def static Assert(condition: Bool) =
        if(!condition) error("Assertion failed.")

    Def static Assert(condition: Bool, message: String) =
        if(!condition) error("Assertion failed: " + message)

    Def static AssertTrue(condition: Bool) = Assert(condition)
    Def static AssertTrue(condition: Bool, message: String) = Assert(condition, message)
    Def static AssertFalse(condition: Bool) = Assert(!condition)
    Def static AssertFalse(condition: Bool, message: String) = Assert(!condition, message)

    Def static AssertEquals(actual: T?, expected: T?) = {
        if(actual && expected && actual != expected)
            error("Assertion failed: '" + actual + "' did not equal '" + expected + "'.")

        if(!actual && expected)
            error("Assertion failed: 'null' did not equal '" + expected + "'.")

        if(actual && !expected)
            error("Assertion failed: '" + actual + "' did not equal 'null'.")
    }


    Def static AssertNotEquals(actual: T?, expected: T?) = {
        if(actual && expected && actual == expected)
            error("Assertion failed: '" + actual + "' was equal to '" + expected + "'.")

        if(!actual && !expected)
            error("Assertion failed: 'null' was equal to 'null'.")
    }

    Def static Fail() = error("Test failed!")
    Def static Fail(message: String) = error("Test failed: " + message)

}