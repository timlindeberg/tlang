package tlang
package compiler
package ast

import tlang.compiler.analyzer.Symbols.VariableSymbol
import tlang.compiler.analyzer.Types
import tlang.compiler.ast.Trees._
import tlang.testutils.{TreeTesting, UnitSpec}
import tlang.utils.StringSource

class CopierSpec extends UnitSpec with TreeTesting {

  behavior of "A tree copier"

  it should "clone trees" in {
    val classId = ClassID("Clazzy")
    val varX = VariableID("X")
    val varXInitiation = IntLit(0)
    val varXDecl = VarDecl(varX, initiation = varXInitiation)

    val varYInitiation = IntLit(10)
    val varY = VariableID("y")
    val plusLhs = IntLit(5)
    val plusRhs = IntLit(6)
    val plus = Plus(plusLhs, plusRhs)
    val varYDecl = VarDecl(varY, initiation = varXInitiation)
    val block = Block(List(plus, varYDecl))
    val methodDecl = MethodDecl("Method1", stat = block)

    val fields = List(varXDecl)
    val methods = List(methodDecl)
    val classDecl = ClassDecl(classId, fields = fields, methods = methods)

    val copier = new Copier()

    val copy = copier.ClassDecl(classDecl, classId, fields = fields, methods = methods)

    copy shouldBe classDecl
    copy should not be theSameInstanceAs(classDecl)
  }

  it should "transfer attributes" in {
    val source = StringSource("ABC", "")
    val symbol = new VariableSymbol("y")
    val varY = VariableID("y")
      .setSymbol(symbol)
      .setType(Types.Int)
      .setPos(source, 1, 2, 3, 4)

    val copier = new Copier()

    val copy: VariableID = copier.VariableID(varY, "y")

    copy.getSymbol should be theSameInstanceAs symbol
    copy.getType shouldBe Types.Int
    copy.source shouldBe Some(source)
    copy.line shouldBe 1
    copy.col shouldBe 2
    copy.lineEnd shouldBe 3
    copy.colEnd shouldBe 4
  }

  behavior of "A lazy tree copier"

  it should "not copy a tree if all children are the same" in {
    val lazyCopier = new LazyCopier()
    val classId = ClassID("Clazzy")
    val classIdCopy = lazyCopier.ClassID(classId, "Clazzy2")

    classId should not be lazyCopier.ClassID(classId, "Clazzy2")
    classId should be theSameInstanceAs lazyCopier.ClassID(classId, "Clazzy")

    val varX = VariableID("X")
    val varXInitiation = IntLit(0)
    val varXDecl = VarDecl(varX, initiation = varXInitiation)

    val varYInitiation = IntLit(10)
    val varY = VariableID("y")
    val plusLhs = IntLit(5)
    val plusRhs = IntLit(6)
    val plus = Plus(plusLhs, plusRhs)
    val varYDecl = VarDecl(varY, initiation = varXInitiation)
    val block = Block(List(plus, varYDecl))
    val methodDecl = MethodDecl("Method1", stat = block)

    val fields = List(varXDecl)
    val methods = List(methodDecl)
    val parents = Nil
    val classDecl = ClassDecl(classId, parents, fields, methods)

    classDecl should be theSameInstanceAs lazyCopier.ClassDecl(classDecl, classId, parents, fields, methods)
    classDecl should not be lazyCopier.ClassDecl(classDecl, ClassID("Clazzy2"), parents, fields, methods)
    classDecl should not be lazyCopier.ClassDecl(classDecl, classId, parents, Nil, methods)
    classDecl should not be lazyCopier.ClassDecl(classDecl, classId, parents, fields, Nil)
    classDecl should not be lazyCopier.ClassDecl(classDecl, classId, List(ClassID("A")), fields, methods)
  }
}
