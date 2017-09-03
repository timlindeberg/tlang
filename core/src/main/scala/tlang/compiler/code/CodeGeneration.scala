package tlang.compiler
package code

import java.io.{BufferedInputStream, FileInputStream, FileOutputStream}

import better.files.File
import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes._
import cafebabe.Flags._
import cafebabe._
import org.objectweb.asm.{ClassReader, ClassWriter}
import tlang.Context
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._
import tlang.formatting.Formatting
import tlang.utils.Extensions._
import tlang.utils.{FileSource, StringSource}

import scala.collection.mutable

object CodeGeneration extends CompilerPhase[CompilationUnit, CodegenerationStackTrace] {

  import CodeGenerator._

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CodegenerationStackTrace] = {
    val classes = cus.flatMap(_.classes)

    // output code in parallell?
    val genResults = classes.map(generateClassFile(_, ctx))

    genResults.foreach(_.files.foreach(generateStackMapFrames))
    genResults.flatMap(_.stackTraces)
  }

  override def description(formatting: Formatting): String =
    "Generates bytecode that can run on the JVM."

  override def printDebugOutput(output: List[CodegenerationStackTrace], debugOutputFormatter: DebugOutputFormatter): Unit =
    debugOutputFormatter.printStackTraces(phaseName, output)

  case class GenerateClassResult(files: Set[String], stackTraces: List[CodegenerationStackTrace])

  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  private def generateClassFile(classDecl: ClassDeclTree, ctx: Context): GenerateClassResult = {
    val classFile = makeClassFile(classDecl)
    classDecl.fields.foreach { varDecl =>
      val varSymbol = varDecl.getSymbol
      val flags = getFieldFlags(varDecl)
      classFile.addField(varSymbol.getType.byteCodeName, varSymbol.name).setFlags(flags)
    }

    initializeStaticFields(classDecl, classFile)

    if (!classDecl.methods.exists(_.isInstanceOf[ConstructorDecl]))
      generateDefaultConstructor(classFile, classDecl)

    val stackTraces = generateMethods(ctx, classDecl, classFile)

    val className = classDecl.getSymbol.JVMName
    val files = ctx.outDirs.map(getFilePath(_, className))
    files.foreach(classFile.writeToFile)
    GenerateClassResult(files, stackTraces)
  }

  private def generateMethods(ctx: Context, classDecl: ClassDeclTree, classFile: ClassFile): List[CodegenerationStackTrace] = {
    classDecl.methods.flatMap { methodDecl =>
      val methSymbol = methodDecl.getSymbol

      val methodHandle = methodDecl match {
        case _: MethodDecl =>
          val argTypes = methSymbol.argTypes.map(_.byteCodeName).mkString
          val methDescriptor = methodDescriptor(methSymbol)
          classFile.addMethod(methSymbol.getType.byteCodeName, methSymbol.name, argTypes, methDescriptor)

        case con: ConstructorDecl =>
          generateConstructor(Some(con), classFile, classDecl)
        case _                    => ???
      }
      val flags = getMethodFlags(methodDecl)
      methodHandle.setFlags(flags)
      methSymbol.annotations foreach methodHandle.addAnnotation

      if (!methodDecl.isAbstract) {
        val ch = generateMethod(methodHandle, methodDecl)

        val classSymbol = classDecl.getSymbol
        // If a method is overriden but with another return type
        // a bridge method needs to be generated
        classSymbol.overriddenMethod(methSymbol)
          .filter(_.getType != methSymbol.getType)
          .ifDefined { overriden =>
            val flags = METHOD_ACC_PUBLIC | METHOD_ACC_BRIDGE | METHOD_ACC_SYNTHETIC

            val thisTree = This().setSymbol(classSymbol).setType(TObject(classSymbol))
            generateBridgeMethod(classFile, overriden, methSymbol, flags, thisTree)
          }

        Some(ch.stackTrace(ctx.formatting))
      } else {
        None
      }
    }
  }

  private def generateObjectInterfaceBridgeMethods() = {
    // TODO: Generate methods so that toString etc. can be defined in an interface
    /*
    if(!classSymbol.isAbstract){
      Types.ObjectSymbol.methods.foreach { objMeth =>
        if(!classSymbol.methods.exists(m => m.name == objMeth.name && m.argTypes == objMeth.argTypes)){
          classSymbol.implementingMethod(objMeth)
            .filter(_.classSymbol.isAbstract)
            .ifDefined{ methodInTrait =>
              // Object method is not defined in the class but is defined in a parent trait
              // A bridge method needs to be generated.
              val trai = methodInTrait.classSymbol
              val base = Super(Some(ClassID(trai.name).setSymbol(trai))).setType(TObject(trai))
              generateBridgeMethod(classFile, objMeth, methodInTrait, METHOD_ACC_PUBLIC, base)
            }
        }
      }
    }
    */
  }

  private def generateBridgeMethod(classFile: ClassFile, overriden: MethodSymbol, meth: MethodSymbol, flags: U2, base: ExprTree) = {
    val argTypes = overriden.argTypes.map(_.byteCodeName).mkString
    val retType = overriden.getType.byteCodeName

    val descriptor = methodDescriptor(overriden)
    val mh = classFile.addMethod(retType, overriden.name, argTypes, descriptor)
    mh.setFlags(flags)

    val args = overriden.argList.map(arg => VariableID(arg.name).setSymbol(arg))

    // The code to generate in the bridge method
    val methType = meth.getType
    val methodID = MethodID(meth.name).setSymbol(meth)
    val methodCall = MethodCall(methodID, args).setType(methType)
    val access = NormalAccess(base, methodCall).setType(methType)
    val code = Return(Some(access)).setType(methType)

    val localVariableMap = constructVariableMap(overriden)

    val ch = mh.codeHandler
    val codeGenerator = new CodeGenerator(ch, localVariableMap)
    codeGenerator.compileStat(code)
    ch.use(_.freeze)
  }

  private def generateStackMapFrames(file: String) = {
    // Use ASM library to generate the stack map frames
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
    // java.lang.Object is already implicitly a parent of all classes
    val parents = classSymbol.parents.filter(_ != ObjectSymbol)
    val className = classSymbol.JVMName

    val (parent, traits) = if (classSymbol.isAbstract)
      (None, parents)
    else if (parents.isEmpty)
      (None, List())
    else if (parents.head.isAbstract)
      (None, parents)
    else
      (Some(parents.head.JVMName), parents.drop(1))

    val classFile = new ClassFile(className, parent)
    traits.foreach(t => classFile.addInterface(t.JVMName))

    classDecl.source ifDefined {
      case FileSource(file)            => classFile.setSourceFile(file.name)
      case StringSource(str, mainName) => classFile.setSourceFile(mainName)
    }

    val flags = if (classSymbol.isAbstract) TraitFlags else ClassFlags
    classFile.setFlags(flags)
    // Default is public

    classFile
  }

  private def generateMethod(mh: MethodHandler, methTree: MethodDeclTree): CodeHandler = {
    val localVariableMap = constructVariableMap(methTree.getSymbol)

    val ch = mh.codeHandler
    val codeGenerator = new CodeGenerator(ch, localVariableMap)
    codeGenerator.compileStat(methTree.stat.get)
    addReturnStatement(ch, methTree.getSymbol)
    ch.use(_.freeze)
  }

  private def constructVariableMap(meth: MethodSymbol): mutable.Map[VariableSymbol, Int] = {
    var offset = if (meth.isStatic) 0 else 1

    mutable.Map() ++ meth.argList.zipWithIndex.map { case (arg, i) =>
      val pair = arg -> (i + offset)
      // Longs and doubles take up two slots
      if (arg.getType.size == 2)
        offset += 1
      pair
    }.toMap
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
        val argTypes = conDecl.getSymbol.argTypes.map(_.byteCodeName).mkString
        val methDescriptor = methodDescriptor(conDecl.getSymbol)
        classFile.addConstructor(argTypes, methDescriptor)
      case _             =>
        classFile.addConstructor("", "new()")
    }

    initializeNonStaticFields(classDecl, mh.codeHandler)
    addSuperCall(mh, classDecl)
    mh
  }

  private def methodDescriptor(methSym: MethodSymbol) = {
    methSym.classSymbol.JVMName + "." + methSym.signature + ":" + methSym.byteCodeSignature
  }

  private def initializeStaticFields(classDecl: ClassDeclTree, classFile: ClassFile): Unit = {
    val staticFields = classDecl.fields.filter(v => v.initation.isDefined && v.isStatic)
    if (staticFields.isEmpty)
      return

    // TODO: why lazy?
    lazy val ch: CodeHandler = classFile.addClassInitializer.codeHandler
    val codeGenerator = new CodeGenerator(ch, mutable.HashMap())
    staticFields.foreach {
      case VarDecl(_, id, Some(expr), _) => compileField(expr, id, classDecl, ch, codeGenerator)
      case _                             =>
    }
    ch << RETURN
    ch.freeze
  }

  private def initializeNonStaticFields(classDecl: ClassDeclTree, ch: CodeHandler) = {
    val nonStaticFields = classDecl.fields.filter(v => v.initation.isDefined && !v.isStatic)
    val codeGenerator = new CodeGenerator(ch, mutable.HashMap())
    nonStaticFields foreach {
      case VarDecl(_, id, Some(expr), _) =>
        ch << ArgLoad(0) // put this-reference on stack
        compileField(expr, id, classDecl, ch, codeGenerator)
      case _                             =>
    }
  }

  private def compileField(expr: ExprTree, id: VariableID, classDecl: ClassDeclTree, ch: CodeHandler, codeGenerator: CodeGenerator) = {
    codeGenerator.compileExpr(expr)
    val sym = id.getSymbol
    val className = classDecl.getSymbol.JVMName
    val fieldName = id.getSymbol.name
    val typeName = sym.getType.byteCodeName
    if (sym.isStatic)
      ch << PutStatic(className, fieldName, typeName)
    else
      ch << PutField(className, fieldName, typeName)

  }

  private def getMethodFlags(method: MethodDeclTree) = {
    var flags: U2 = 0

    method.modifiers.foreach {
      case Public()    => flags |= METHOD_ACC_PUBLIC
      case Private()   => flags |= METHOD_ACC_PRIVATE
      case Protected() => flags |= METHOD_ACC_PROTECTED
      case Static()    => flags |= METHOD_ACC_STATIC
      case _           =>
    }
    if (method.isAbstract)
      flags |= METHOD_ACC_ABSTRACT

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

  private def getFilePath(outDir: File, className: String): String = {
    var prefix = outDir.pathAsString.replaceAll("\\\\", "/")

    // Weird Windows behaviour
    if (prefix.endsWith("."))
      prefix = prefix.dropRight(1)

    prefix += "/"
    val split = className.split("/")
    val packageDir = split.dropRight(1).mkString("/")
    val filePath = prefix + packageDir
    val f = File(filePath)
    f.createIfNotExists()

    prefix + className + ".class"
  }

  private def addReturnValueAndStatement(ch: CodeHandler, tpe: Type) = tpe match {
    case TUnit => ch << RETURN
    case _     =>
      tpe.codes.defaultConstant(ch)
      tpe.codes.ret(ch)
  }

  private def addReturnStatement(ch: CodeHandler, methodSymbol: MethodSymbol) = ch.lastRealInstruction match {
    case Some(byteCode) =>
      byteCode match {
        case ARETURN | IRETURN | RETURN | DRETURN | FRETURN | LRETURN =>
        case _                                                        =>
          addReturnValueAndStatement(ch, methodSymbol.getType)
      }
    case None           => ch << RETURN
  }

  private def addSuperCall(mh: MethodHandler, classDecl: ClassDeclTree) = {
    val superClassName = classDecl.getSymbol.parents match {
      case (c: ClassSymbol) :: _ => if (c.isAbstract) JavaObject else c.JVMName
      case Nil                   => JavaObject
    }

    mh.codeHandler << ALOAD_0
    mh.codeHandler << InvokeSpecial(superClassName, CodeGenerator.ConstructorName, "()V")
  }

}



