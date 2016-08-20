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
import tcompiler.utils.Extensions._

import scala.collection.mutable

object CodeGeneration extends Pipeline[List[CompilationUnit], Unit] with Colorizer {

  import CodeGenerator._
  var useColor = false

  def run(ctx: Context)(cus: List[CompilationUnit]): Unit = {
    val classes = cus.flatMap(_.classes)

    // output code in parallell?
    useColor = ctx.useColor
    ctx.printCodeStage ifDefined { stage =>
      if(stage == CodeGeneration.stageName){
        val stageName = Blue(stage.capitalize)
        println(s"${Bold}Output after $Reset$stageName:\n")
      }
    }
    val outputFiles = classes.map(generateClassFile(_, ctx))
    outputFiles foreach generateStackMapFrames
  }

  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  private def generateClassFile(classDecl: ClassDeclTree, ctx: Context): String = {
    val classFile = makeClassFile(classDecl)
    classDecl.fields.foreach { varDecl =>
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
          val signature = classDecl.getSymbol.name + "." + mt.signature
          classFile.addMethod(methSymbol.getType.byteCodeName, methSymbol.name, argTypes, signature)
        case con: ConstructorDecl =>
          hasConstructor = true
          generateConstructor(Some(con), classFile, classDecl)
      }
      var flags: U2 = getMethodFlags(methodDecl)
      if (methodDecl.isAbstract)
        flags |= METHOD_ACC_ABSTRACT
      methodHandle.setFlags(flags)

      if(!methodDecl.isAbstract){
        val ch = generateMethod(methodHandle, methodDecl)
        ctx.printCodeStage ifDefined { stage =>
          if(stage == CodeGeneration.stageName)
            println(ch.stackTrace(ctx.useColor))

        }
      }

    }

    if (!hasConstructor)
      generateDefaultConstructor(classFile, classDecl)

    val className = classDecl.getSymbol.name
    val file = getFilePath(ctx.outDir, className)
    classFile.writeToFile(file)

    file
  }

  private def generateStackMapFrames(file: String) = {
    // Use ASM libary to generate the stack map frames
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

  private def makeClassFile(classDecl: ClassDeclTree) = {
    val classSymbol = classDecl.getSymbol
    val parents = classSymbol.parents
    val className = classSymbol.name

    val (parent, traits) = if (classSymbol.isAbstract)
      (None, parents)
    else if (parents.isEmpty)
      (None, List())
    else if (parents.head.isAbstract)
      (None, parents)
    else
      (Some(parents.head.name), parents.drop(1))

    val classFile = new ClassFile(className, parent)
    traits.foreach(t => classFile.addInterface(t.name))
    classFile.setSourceFile(classDecl.file.getName)

    val flags = if (classSymbol.isAbstract) TraitFlags else ClassFlags
    classFile.setFlags(flags)
    // Defualt is public

    classFile
  }

  private def generateMethod(mh: MethodHandler, methTree: FuncTree): CodeHandler = {
    val localVariableMap = mutable.HashMap[VariableSymbol, Int]()

    var offset = if (methTree.isStatic) 0 else 1

    methTree.args.zipWithIndex.foreach {
      case (arg, i) =>
        localVariableMap(arg.getSymbol) = i + offset
        if (arg.getSymbol.getType.size == 2) {
          // Longs and doubles take up two slots
          offset += 1
        }
    }

    val ch = mh.codeHandler
    val codeGenerator = new CodeGenerator(ch, localVariableMap)
    codeGenerator.compileStat(methTree.stat.get)
    addReturnStatement(ch, methTree)
    ch.freeze
    ch
  }

  private def generateDefaultConstructor(classFile: ClassFile, classDecl: ClassDeclTree): Unit = {
    if (classDecl.getSymbol.isAbstract)
      return

    val mh = generateConstructor(None, classFile, classDecl)
    val ch = mh.codeHandler
    ch << RETURN
    ch.freeze
  }

  private def generateConstructor(con: Option[ConstructorDecl], classFile: ClassFile, classDecl: ClassDeclTree): MethodHandler = {
    val mh = con match {
      case Some(conDecl) =>
        val argTypes = conDecl.getSymbol.argList.map(_.getType.byteCodeName).mkString
        val signature = classDecl.getSymbol.name + "." + conDecl.signature
        classFile.addConstructor(argTypes, signature)
      case _             =>
        classFile.addConstructor("", "new()")
    }

    initializeNonStaticFields(classDecl, mh.codeHandler)
    addSuperCall(mh, classDecl)
    mh
  }

  private def initializeStaticFields(classDecl: ClassDeclTree, classFile: ClassFile): Unit = {
    val staticFields = classDecl.fields.filter(v => v.init.isDefined && v.isStatic)
    if (staticFields.isEmpty)
      return

    // TODO: why lazy?
    lazy val ch: CodeHandler = classFile.addClassInitializer.codeHandler
    val codeGenerator = new CodeGenerator(ch, mutable.HashMap())
    staticFields.foreach { case varDecl@VarDecl(varTpe, id, Some(expr), _) =>
      compileField(expr, id, classDecl, ch, codeGenerator)
    }
    ch << RETURN
    ch.freeze
  }

  private def initializeNonStaticFields(classDecl: ClassDeclTree, ch: CodeHandler) = {
    val nonStaticFields = classDecl.fields.filter(v => v.init.isDefined && !v.isStatic)
    val codeGenerator = new CodeGenerator(ch, mutable.HashMap())
    nonStaticFields foreach { case varDecl@VarDecl(_, id, Some(expr), _) =>
      ch << ArgLoad(0) // put this-reference on stack
      compileField(expr, id, classDecl, ch, codeGenerator)
    }
  }

  private def compileField(expr: ExprTree, id: VariableID, classDecl: ClassDeclTree, ch: CodeHandler, codeGenerator: CodeGenerator) = {
    codeGenerator.compileExpr(expr)
    val sym = id.getSymbol
    val className = classDecl.getSymbol.name
    val fieldName = id.getSymbol.name
    val typeName = sym.getType.byteCodeName
    if(sym.isStatic)
      ch << PutStatic(className, fieldName, typeName)
    else
      ch << PutField(className, fieldName, typeName)

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
      case Final()     => flags |= FIELD_ACC_FINAL
      case _           =>
    }
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

  private def addSuperCall(mh: MethodHandler, classDecl: ClassDeclTree) = {
    val superClassName = classDecl.getSymbol.parents match {
      case (c: ClassSymbol) :: _ => if (c.isAbstract) JavaObject else c.name
      case Nil                   => JavaObject
    }

    mh.codeHandler << ALOAD_0
    mh.codeHandler << InvokeSpecial(superClassName, CodeGenerator.ConstructorName, "()V")
  }

}



