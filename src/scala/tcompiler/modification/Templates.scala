package tcompiler
package modification

import com.rits.cloning._
import tcompiler.ast.Trees
import tcompiler.ast.Trees._
import tcompiler.imports.TemplateImporter
import tcompiler.utils.{Context, Pipeline, Positioned}

import scala.collection.mutable.ArrayBuffer

object Templates extends Pipeline[List[Program], List[Program]] {

  val StartEnd = "-"
  val Seperator = "$"

  def run(ctx: Context)(progs: List[Program]): List[Program] = {

    def templatedClasses(prog:Program) = prog.classes.filter(_.id.isTemplated)

    progs.map { prog =>
      val templateImporter = new TemplateImporter(ctx, prog)
      val templateProgs = templateImporter()

      val templateClasses = templatedClasses(prog) ::: templateProgs.flatMap(templatedClasses)
      val newClasses = new ClassGenerator(ctx, prog, templateClasses).generate()
      val oldClasses = prog.classes.filter(!_.id.isTemplated)
      val newProg = prog.copy(classes = oldClasses ++ newClasses)
      newProg.setPos(prog)
      replaceTypes(newProg)
    }
  }


  private def replaceTypes(prog: Program): Program = {
    def replaceType(tpe: TypeTree): TypeTree = tpe match {
      case x: ClassIdentifier    => replaceTypeId(x)
      case x @ ArrayType(arrTpe) =>
        x.tpe = replaceType(arrTpe)
        x
      case x                     => x
    }

    def replaceTypeId(tpe: ClassIdentifier) =
      if (tpe.isTemplated) new ClassIdentifier(tpe.templatedClassName).setPos(tpe)
      else tpe

    Trees.traverse(prog, (_, curr) => Some(curr) collect {
      case c: ClassDecl    => c.parents = c.parents.map(replaceTypeId)
      case m: MethodDecl   => m.retType collect { case t => m.retType = Some(replaceType(t))}
      case o: OperatorDecl => o.retType collect { case t => o.retType = Some(replaceType(t))}
      case v: VarDecl      => v.tpe collect { case t => v.tpe = Some(replaceType(t))}
      case f: Formal       => f.tpe = replaceType(f.tpe)
      case n: NewArray     => n.tpe = replaceType(n.tpe)
      case n: New          => n.tpe = replaceType(n.tpe)
    })
    prog
  }
}

class ClassGenerator(ctx: Context, prog: Program, templateClasses: List[ClassDecl]) {

  private val cloner = new Cloner
  cloner.registerConstant(Nil)
  cloner.registerConstant(None)

  private var generated: Set[String] = Set()
  private var generatedClasses: ArrayBuffer[ClassDecl] = ArrayBuffer()


  def generate(): List[ClassDecl] = {

    def generateIfTemplated(tpe: TypeTree): Unit = {
      Some(tpe) collect {
        case x: ClassIdentifier if x.isTemplated =>
          x.templateTypes.foreach(generateIfTemplated)
          generateClass(x)
      }
    }

    def collect(f: Tree, p: Tree) = Some(p) collect {
      case c: ClassDecl => c.parents.foreach(generateIfTemplated)
      case v: VarDecl   => v.tpe collect { case t => generateIfTemplated(t) }
      case f: Formal    => generateIfTemplated(f.tpe)
      case n: New       => generateIfTemplated(n.tpe)
    }

    checkTemplateClassDefs(templateClasses)
    prog.classes.foreach(clazz => Trees.traverse(clazz, collect))
    generatedClasses.toList
  }

  private def generateClass(typeId: ClassIdentifier): Unit = {
    if (generated(typeId.templatedClassName))
      return


    generated += typeId.templatedClassName

    templateClasses.find(_.id.value == typeId.value) match {
      case Some(template) => generatedClasses += newTemplateClass(template, typeId)
      case None           =>
        ErrorDoesNotExist(typeId.value, typeId)
    }
  }

  private def newTemplateClass(template: ClassDecl, typeId: ClassIdentifier): ClassDecl = {

    val templateTypes = typeId.templateTypes
    val templateMap = constructTemplateMapping(typeId, template.id.templateTypes, templateTypes)

    /* Helper functions to perform transformation */
    def updateType(t: TypeTree): TypeTree = {
      Some(t) collect {
        case t @ ClassIdentifier(_, templateTypes) if t.isTemplated =>
          t.templateTypes = templateTypes.map(updateType)
          generateClass(t)
        case a @ ArrayType(tpe)                                     => a.tpe = updateType(tpe)
      }
      templateMap.getOrElse(t, t)
    }

    def templateName(id: ClassIdentifier) =
      id.copy(value = template.id.templatedClassName(templateTypes), templateTypes = List())

    val newClass = cloner.deepClone(template)
    Trees.traverse(newClass, (_, current) => Some(current) collect {
      case c: ClassDecl       =>
        c.id = templateName(c.id)
        c.parents = c.parents.map(p => updateType(p).asInstanceOf[ClassIdentifier])
      case v: VarDecl         => v.tpe collect { case t => v.tpe = Some(updateType(t)) }
      case f: Formal          => f.tpe = updateType(f.tpe)
      case m: MethodDecl      => m.retType collect { case t => m.retType = Some(updateType(t))}
      case o: OperatorDecl    => o.retType collect { case t => o.retType = Some(updateType(t))}
      case n: New             => n.tpe = updateType(n.tpe)
      case n: NewArray        => n.tpe = updateType(n.tpe)
    })
    newClass
  }

  private def constructTemplateMapping(typedId: ClassIdentifier, templateList: List[TypeTree], templateTypes: List[TypeTree]): Map[TypeTree, TypeTree] = {
    val diff = templateTypes.size - templateList.size
    if (diff != 0) {
      ErrorWrongNumGenerics(templateList.size, templateTypes.size, typedId)
    } else {
      templateList.zip(templateTypes).toMap
    }
  }

  private def checkTemplateClassDefs(templateClasses: List[ClassDecl]) =
    templateClasses.foreach { tClass =>
      var set = Set[TypeTree]()
      var reportedFor = Set[TypeTree]()
      val templateTypes = tClass.id.templateTypes
      templateTypes.foreach { tType =>
        if (set(tType) && !reportedFor(tType)) {
          ErrorSameName(tType.name, tType)
          reportedFor += tType
        }
        set += tType
      }
    }

  private val ErrorMap = Map[TypeTree, TypeTree]()
  private val ErrorType = new ClassIdentifier("ERROR")
  private val ErrorClass = new ClassDecl(ErrorType, List(), List(), List(), false)

  private def error(errorCode: Int, msg: String, pos: Positioned): Unit =
    ctx.reporter.error("G", errorCode, msg, pos)

  //---------------------------------------------------------------------------------------
  //  Error messages
  //---------------------------------------------------------------------------------------

  private def ErrorWrongNumGenerics(expected: Int, found: Int, pos: Positioned) = {
    error(0, s"Wrong number of template parameters, expected '$expected', found '$found'.", pos)
    ErrorMap
  }

  private def ErrorDoesNotExist(name: String, pos: Positioned) = {
    error(1, s"Can not find template class named '$name'.", pos)
    ErrorClass
  }

  private def ErrorSameName(name: String, pos: Positioned) = {
    error(2, s"Generic parameter duplicate: '$name'.", pos)
  }
}
