# Imports
A class is imported using the following syntax:

```tlang
import java::lang::StringBuilder
import t::std::Vector
```

Importing classes written in `Java` or other JVM languages is just as easy as
importing a `t`-class which opens up a whole world of useful libraries and code.
One can also use wild card imports to import all the classes within a package.

```tlang
import foo::bar::* // Imports all classes in the foo::bar package
```
## Packages
Packages are declared using the `package` key word:
```tlang
package foo::bar

class A

// Can be imported in other files by typing:
import foo::bar::A
```

## Importing extension classes
To import an extension class one has to specify the class to be extended and
the package in which the the extension class is defined:

```tlang
// Imports an extension to java::lang::String defined in the foo::bar package 
import java::lang::String::extension foo::bar
```

This brings the extension class into scope and allows its defined methods
to be applied within the file.


## Importing generic classes
Since the compiler needs access to the code to instantiate a new instance of the
class the semantics of importing a generic class is slightly different to importing
a regular class. Fortunately most of this is hidden for the user, generic
classes are imported the same way as regular classes. However, this means that
generic and regular classes can't be declared in the same file. It also means
that to package a library written in `tlang`, the actual code of the generic
classes has to be bundled. 
