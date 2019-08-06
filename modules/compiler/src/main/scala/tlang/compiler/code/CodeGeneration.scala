package tlang
package compiler
package code

import java.io.{BufferedInputStream, FileInputStream, FileOutputStream}
import java.net.URLClassLoader

import better.files._
import cafebabe.AbstractByteCodes._
import cafebabe.ByteCodes._
import cafebabe.ClassFileTypes._
import cafebabe.Flags._
import cafebabe._
import org.objectweb.asm.{ClassReader, ClassWriter}
import tlang.compiler.analyzer.Symbols
import tlang.compiler.analyzer.Symbols._
import tlang.compiler.analyzer.Types._
import tlang.compiler.ast.Trees._
import tlang.compiler.output.Output
import tlang.compiler.output.debug.CodeGenerationOutput
import tlang.formatting.Formatter
import tlang.utils.FileSource

import scala.collection.mutable

object CodeGeneration extends CompilerPhase[CompilationUnit, CodegenerationStackTrace] {

  import CodeGenerator._

  def run(ctx: Context)(cus: List[CompilationUnit]): List[CodegenerationStackTrace] = {
    val classes = cus.flatMap(_.classes)
    val results = ctx.executor.map(classes) { generateClassFile(_, ctx) }

    val extraClassPaths = ctx.allClassPaths.map(File(_).url).toArray
    val classLoader = URLClassLoader.newInstance(extraClassPaths)

    ctx.executor.flatMap(results) { case Result(files, stackTraces) =>
      //files.foreach(generateStackMapFrames(_, classLoader))
      stackTraces
    }
  }

  override def description(implicit formatter: Formatter): String =
    "Generates bytecode that can run on the JVM."

  override def debugOutput(output: List[CodegenerationStackTrace])(implicit formatter: Formatter): Output =
    CodeGenerationOutput(phaseName, output)


  case class Result(files: Set[String], stackTraces: List[CodegenerationStackTrace])
  /** Writes the proper .class file in a given directory. An empty string for dir is equivalent to "./". */
  private def generateClassFile(classDecl: ClassDeclTree, ctx: Context): Result = {
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
    val files = ctx.outDirs.map { classFilePath(_, className) }
    files.foreach { file =>
      info"Creating .class file $file"
      classFile.writeToFile(file)
    }
    Result(files, stackTraces)
  }

  private def generateMethods(ctx: Context, classDecl: ClassDeclTree, classFile: ClassFile): List[CodegenerationStackTrace] = {
    import ctx.formatter

    classDecl.methods.flatMap { methodDecl =>
      val methSymbol = methodDecl.getSymbol

      val methodHandle = methodDecl match {
        case _: MethodDecl =>
          val argTypes = methSymbol.argTypes.map(_.byteCodeName).mkString
          val methDescriptor = methodDescriptor(methSymbol)
          classFile.addMethod(
            methSymbol.getType.byteCodeName,
            methSymbol.name,
            argTypes,
            methodDecl.isAbstract,
            methDescriptor
          )

        case con: ConstructorDecl => generateConstructor(Some(con), classFile, classDecl)
        case _                    => ???
      }
      val flags = getMethodFlags(methodDecl)
      methodHandle.setFlags(flags)
      methSymbol.annotations foreach { addAnnotation(methodHandle, _) }

      if (!methodDecl.isAbstract) {
        val ch = generateMethod(methodHandle, methodDecl)

        val classSymbol = classDecl.getSymbol
        // If a method is overriden but with another return type
        // a bridge method needs to be generated
        classSymbol.overriddenMethod(methSymbol)
          .filter { _.getType != methSymbol.getType }
          .ifDefined { overriden =>
            val flags = METHOD_ACC_PUBLIC | METHOD_ACC_BRIDGE | METHOD_ACC_SYNTHETIC

            val thisTree = This().setSymbol(classSymbol).setType(TObject(classSymbol))
            generateBridgeMethod(classFile, overriden, methSymbol, flags, thisTree)
          }

        Some(ch.stackTrace)
      } else {
        None
      }
    }
  }

  private def addAnnotation(annotatable: cafebabe.Annotatable, annotation: Symbols.AnnotationSymbol): Unit = {
    val annotationHandler = annotatable.addAnnotation(annotation.getType.byteCodeName)
    annotation.elements foreach { case (name, v) =>
      v match {
        case IntAnnotationValue(v)    => annotationHandler.addValue(name, v)
        case LongAnnotationValue(v)   => annotationHandler.addValue(name, v)
        case FloatAnnotationValue(v)  => annotationHandler.addValue(name, v)
        case DoubleAnnotationValue(v) => annotationHandler.addValue(name, v)
        case StringAnnotationValue(v) => annotationHandler.addValue(name, v)
        case _                        => ???
      }
    }
  }

  private def generateObjectInterfaceBridgeMethods(): Unit = {
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
    val mh = classFile.addMethod(retType, overriden.name, argTypes, isAbstract = false, descriptor)
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

  private def generateStackMapFrames(file: String, classLoader: ClassLoader): Unit = {
    // Use ASM library to generate the stack map frames
    // since Cafebabe does not support this.

    // In some cases BCEL can need access to the generated classes when generating
    // stack map frames. We use a new class loader with access to the newly
    // generated classes .
    val classWriter = new ClassWriterWithCustomClassLoader(classLoader, ClassWriter.COMPUTE_FRAMES)
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
    traits.foreach { t => classFile.addInterface(t.JVMName) }

    classDecl.source partialMatch {
      case Some(FileSource(file)) => classFile.setSourceFile(file.name)
    }

    val isAnnotation = classDecl.isInstanceOf[AnnotationDecl]

    val flags = if (isAnnotation)
      AnnotationFlags
    else if (classSymbol.isAbstract)
      TraitFlags
    else
      ClassFlags


    classFile.setFlags(flags)
    // Default is public

    classSymbol.annotations foreach { addAnnotation(classFile, _) }
    classFile
  }

  private def generateMethod(mh: MethodHandler, methTree: MethodDeclTree): CodeHandler = {
    debug"Generating byte code for method ${ methTree.signature }"

    val localVariableMap = constructVariableMap(methTree.getSymbol)

    val ch = mh.codeHandler
    val codeGenerator = new CodeGenerator(ch, localVariableMap)
    codeGenerator.compileStat(methTree.stat.get)
    addReturnStatement(ch, methTree.getSymbol)
    ch.freeze
    ch
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

    addSuperCall(mh, classDecl)
    initializeNonStaticFields(classDecl, mh.codeHandler)
    mh
  }

  private def methodDescriptor(methSym: MethodSymbol) = {
    val signature = methSym.name + methSym.argTypes.mkString("(", ", ", ")")
    methSym.classSymbol.JVMName + "." + signature + "<->" + methSym.byteCodeSignature
  }

  private def initializeStaticFields(classDecl: ClassDeclTree, classFile: ClassFile): Unit = {
    val staticFields = classDecl.fields.filter(v => v.initiation.isDefined && v.isStatic)
    if (staticFields.isEmpty)
      return

    // TODO: why lazy?
    lazy val ch: CodeHandler = classFile.addClassInitializer.codeHandler
    val codeGenerator = new CodeGenerator(ch, mutable.HashMap())
    staticFields foreach { codeGenerator.compileField(classDecl, _) }

    ch << RETURN
    ch.freeze
  }

  private def initializeNonStaticFields(classDecl: ClassDeclTree, ch: CodeHandler): Unit = {
    val nonStaticFields = classDecl.fields.filter(v => v.initiation.isDefined && !v.isStatic)
    val codeGenerator = new CodeGenerator(ch, mutable.HashMap())
    nonStaticFields foreach { codeGenerator.compileField(classDecl, _) }
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

  private def classFilePath(outDir: File, className: String): String = {
    val file = outDir.pathAsString / (className + ".class")
    file.parent.createDirectories()
    file.pathAsString
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



