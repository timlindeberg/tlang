package tlang.compiler.imports

import tlang.compiler.analyzer.Symbols.{ClassSymbol, FieldSymbol, MethodSymbol, OperatorSymbol}

class LazyClassSymbol(classSymbolLocator: ClassSymbolLocator, override val name: String) extends ClassSymbol(name) {

  var loaded = false

  override def methods: List[MethodSymbol] = {
    loadClassSymbol()
    _methods
  }

  override def operators: List[OperatorSymbol] = {
    loadClassSymbol()
    _operators
  }

  override def fields: Map[String, FieldSymbol] = {
    loadClassSymbol()
    _fields
  }

  override def isAbstract: Boolean = {
    loadClassSymbol()
    _isAbstract
  }

  private def loadClassSymbol(): Unit =
    if (!loaded) {
      classSymbolLocator.fillClassSymbol(this)
      loaded = true
    }
}
