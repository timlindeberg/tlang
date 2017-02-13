class Valid : A, B, C {}

class Invalid: B, C, A {} // res: N2017

class A {}

trait B {}

trait C {}