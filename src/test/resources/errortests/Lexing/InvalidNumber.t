prefix 1e-10 postfix
prefix 1e10 postfix
prefix 1.e10 postfix
prefix 1.e10f postfix
prefix 1.e10f postfix
prefix 1____.e10f postfix
prefix 1____.e10f postfix
prefix 1_1_2_3_.e10f postfix

prefix 1e.10 postfix       // res: L2012
prefix 1e1.0 postfix       // res: L2011
prefix 1e1.0f postfix      // res: L2011
prefix 1e.10f postfix      // res: L2012
prefix 1e1.0f postfix      // res: L2011
prefix 1e. postfix         // res: L2012
prefix 1e postfix          // res: L2012
prefix 1e- postfix         // res: L2012
prefix 1e-f postfix        // res: L2012
prefix 1eeeeeeee10 postfix // res: L2012
prefix 1eL postfix         // res: L2012
prefix 1e10L postfix       // res: L2012


prefix 1e10Lasda postfix       // res: L2012