package tcompiler
package code

import java.io.{BufferedInputStream, File, FileInputStream, FileOutputStream}

import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes._
import cafebabe.Flags._
import cafebabe._
import org.objectweb.asm.{ClassReader, ClassWriter}
import tcompiler.analyzer.Symbols._
import tcompiler.analyzer.Types._
import tcompiler.ast.Trees._
import tcompiler.utils._

import scala.collection.mutable

object CodeGeneration extends Pipeline[Program, Unit] {

  import CodeGenerator._

  def run(ctx: Context)(prog: Program): Unit = {
    val sourceName = ctx.file.getName

    // output code in parallell
    prog.classes.par.foreach {
      case c: InternalClassDecl => generateClassFile(sourceName, c, ctx.outDir)
      case c: Trait             => generateClassFile(sourceName, c, ctx.outDir)
      case _                    =>
    }
  }

  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  private def generateClassFile(sourceName: String, classDecl: ClassDecl, dir: Option[File]): Unit = {
    val classFile = makeClassFile(classDecl)

    classDecl.vars.foreach { varDecl =>
      val varSymbol = varDecl.getSymbol
      val flags = getFieldFlags(varDecl)
      classFile.addField(varSymbol.getType.byteCodeName, varSymbol.name).setFlags(flags)
    }

    initializeStaticFields(classDecl, classFile)

    var hasConstructor = false
    classDecl.methods.foreach { methodDecl =>
      val methSymbol = methodDecl.getSymbol

      val methodHandle = methodDecl match {
        case mt: MethodDecl       =>
          val argTypes = methSymbol.argList.map(_.getType.byteCodeName).mkString
          classFile.addMethod(methSymbol.getType.byteCodeName, methSymbol.name, argTypes)
        case con: ConstructorDecl =>
          hasConstructor = true
          generateConstructor(Some(con), classFile, classDecl)
        case op: OperatorDecl     =>
          val argTypes = methSymbol.argList.map(_.getType.byteCodeName).mkString
          val operatorSymbol = op.getSymbol.asInstanceOf[OperatorSymbol]
          classFile.addMethod(methSymbol.getType.byteCodeName, operatorSymbol.methodName, argTypes)
      }
      var flags: U2 = getMethodFlags(methodDecl)
      if (methodDecl.isAbstract)
        flags |= METHOD_ACC_ABSTRACT
      methodHandle.setFlags(flags)
      generateMethod(methodHandle, methodDecl)
    }

    if (!hasConstructor)
      generateDefaultConstructor(classFile, classDecl)

    val className = classDecl.id.value
    val file = getFilePath(dir, className)
    classFile.writeToFile(file)

    generateStackMapFrames(file)
  }

  private def generateStackMapFrames(file: String) = {
    // Uses ASM libary to generate the stack map frames
    // since Cafebabe does not support this.
    val classWriter = new ClassWriter(ClassWriter.COMPUTE_FRAMES)
    val inputStream = new BufferedInputStream(new FileInputStream(file))
    val classReader = new ClassReader(inputStream)

    classReader.accept(classWriter, ClassReader.SKIP_FRAMES)
    inputStream.close()
    val bytes = classWriter.toByteArray

    val fileStream = new FileOutputStream(file)
    fileStream.write(bytes)
    fileStream.close()
  }

  private def makeClassFile(classDecl: ClassDecl) = {
    val classSymbol = classDecl.getSymbol
    val parents = classSymbol.parents
    val className = classSymbol.name

    val (parent, traits) = if (classSymbol.isInstanceOf[TraitSymbol])
      (None, parents)
    else if (parents.isEmpty)
      (None, List())
    else if (parents.head.isInstanceOf[TraitSymbol])
      (None, parents)
    else
      (Some(parents.head.name), parents.drop(1))

    val classFile = new ClassFile(className, parent)
    traits.foreach(t => classFile.addInterface(t.name))
    classFile.setSourceFile(classDecl.file.getName)

    val flags = classSymbol match {
      case _: TraitSymbol => TraitFlags
      case _: ClassSymbol => ClassFlags
    }
    classFile.setFlags(flags)
    // Defualt is public

    classFile
  }

  private def generateMethod(mh: MethodHandler, methTree: FuncTree): Unit = {
    val methSym = methTree.getSymbol
    val variableMap = mutable.HashMap[VariableSymbol, Int]()

    var offset = if (methTree.isStatic) 0 else 1

    methTree.args.zipWithIndex.foreach {
      case (arg, i) =>
        variableMap(arg.getSymbol) = i + offset
        if (arg.getSymbol.getType.size == 2) {
          // Longs and doubles take up two slots
          offset += 1
        }
    }
    if (!methTree.isAbstract) {
      val ch = mh.codeHandler
      val codeGenerator = new CodeGenerator(ch, methSym.classSymbol.name, variableMap)
      codeGenerator.compileStat(methTree.stat.get)
      addReturnStatement(ch, methTree)
      ch.freeze
    }
  }

  private def generateDefaultConstructor(classFile: ClassFile, classDecl: ClassDecl): Unit = {
    if (classDecl.getSymbol.isInstanceOf[TraitSymbol])
      return

    val mh = generateConstructor(None, classFile, classDecl)
    val ch = mh.codeHandler
    ch << RETURN
    ch.freeze
  }

  private def generateConstructor(con: Option[ConstructorDecl], classFile: ClassFile, classDecl: ClassDecl): MethodHandler = {
    val mh = con match {
      case Some(conDecl) =>
        val argTypes = conDecl.getSymbol.argList.map(_.getType.byteCodeName).mkString
        classFile.addConstructor(argTypes)
      case _             =>
        classFile.addConstructor(Nil)
    }

    initializeNonStaticFields(classDecl, mh.codeHandler)
    addSuperCall(mh, classDecl)
    mh
  }

  private def initializeStaticFields(classDecl: ClassDecl, classFile: ClassFile) = {
    val staticFields = classDecl.vars.filter(v => v.init.isDefined && v.isStatic)
    if (staticFields.nonEmpty) {
      lazy val staticCh: CodeHandler = classFile.addClassInitializer.codeHandler
      val codeGenerator = new CodeGenerator(staticCh, classDecl.getSymbol.name, mutable.HashMap())
      staticFields.foreach {
        case varDecl@VarDecl(varTpe, id, Some(expr), _) =>
          varDecl.getSymbol.getType
          codeGenerator.compileExpr(expr)
          staticCh << PutStatic(classDecl.getSymbol.name, id.value, varDecl.getSymbol.getType.byteCodeName)
      }
      staticCh << RETURN
      staticCh.freeze
    }
  }

  private def initializeNonStaticFields(classDecl: ClassDecl, ch: CodeHandler) = {
    val nonStaticFields = classDecl.vars.filter(v => v.init.isDefined && !v.isStatic)
    val codeGenerator = new CodeGenerator(ch, classDecl.getSymbol.name, mutable.HashMap())
    nonStaticFields.foreach {
      case varDecl@VarDecl(_, id, Some(expr), _) =>
        ch << ArgLoad(0) // put this-reference on stack
        codeGenerator.compileExpr(expr)
        ch << PutField(classDecl.getSymbol.name, id.value, varDecl.getSymbol.getType.byteCodeName)
    }
  }

  private def getMethodFlags(method: FuncTree) = {
    var flags: U2 = 0

    method.modifiers.foreach {
      case Public()    => flags |= METHOD_ACC_PUBLIC
      case Private()   => flags |= METHOD_ACC_PRIVATE
      case Protected() => flags |= METHOD_ACC_PROTECTED
      case Static()    => flags |= METHOD_ACC_STATIC
      case _           =>
    }
    flags
  }

  private def getFieldFlags(varDecl: VarDecl) = {
    var flags: U2 = 0

    varDecl.modifiers.foreach {
      case Public()    => flags |= FIELD_ACC_PUBLIC
      case Private()   => flags |= FIELD_ACC_PRIVATE
      case Protected() => flags |= FIELD_ACC_PROTECTED
      case Static()    => flags |= FIELD_ACC_STATIC
      case _           =>
    }

    // TODO: Add final variables. For now all static fields in interfaces are
    // implicitly final
    val classSymbol = varDecl.getSymbol.classSymbol.get
    if(classSymbol.isInstanceOf[TraitSymbol])
      flags |= FIELD_ACC_FINAL


    flags
  }

  private def getFilePath(outDir: Option[File], className: String): String = {
    val prefix = outDir.map(_.getAbsolutePath.replaceAll("\\\\", "/") + "/").getOrElse("")

    val split = className.split("/")
    val packageDir = split.dropRight(1).mkString("/")
    val filePath = prefix + packageDir
    val f = new File(filePath)
    if (!f.getAbsoluteFile.exists() && !f.mkdirs())
      sys.error(s"Could not create output directory '${f.getAbsolutePath}'.")


    prefix + className + ".class"
  }

  private def addReturnValueAndStatement(ch: CodeHandler, tpe: Type) = tpe match {
    case TUnit => ch << RETURN
    case _     =>
      tpe.codes.defaultConstant(ch)
      tpe.codes.ret(ch)
  }

  private def addReturnStatement(ch: CodeHandler, mt: FuncTree) = ch.lastRealInstruction match {
    case Some(byteCode) =>
      byteCode match {
        case ARETURN | IRETURN | RETURN | DRETURN | FRETURN | LRETURN =>
        case _                                                        =>
          mt match {
            case mt: MethodDecl     => addReturnValueAndStatement(ch, mt.getSymbol.getType)
            case op: OperatorDecl   => addReturnValueAndStatement(ch, op.getSymbol.getType)
            case _: ConstructorDecl => ch << RETURN
          }
      }
    case None           => ch << RETURN
  }

  private def addSuperCall(mh: MethodHandler, ct: ClassDecl) = {
    val superClassName = ct.getSymbol.parents match {
      case (_: TraitSymbol) :: tail => JavaObject
      case (c: ClassSymbol) :: tail => c.name
      case Nil                      => JavaObject
    }

    mh.codeHandler << ALOAD_0
    mh.codeHandler << InvokeSpecial(superClassName, CodeGenerator.ConstructorName, "()V")
  }

}



