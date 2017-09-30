package tlang.repl.evaluation

import tlang.compiler.ast.Trees._
import tlang.compiler.imports.Imports
import tlang.formatting.Formatter
import tlang.utils.Extensions._

case class Extractor(formatter: Formatter, state: ReplState) {

  import Evaluator._
  import formatter.formatting._

  // Updates the repl state and returns messages for all new definitions
  def apply(cu: CompilationUnit): List[String] = {
    val replClass = cu.classes.find { _.tpe == ReplClassID }
    extractImports(cu.imports) ++ (replClass match {
      case Some(mainClass) =>
        val classes = cu.classes.remove(mainClass)
        val (methods, stats) = mainClass.methods.find(_.isMain) match {
          case Some(mainMethod) => (mainClass.methods.remove(mainMethod), mainMethod.stat)
          case None             => (mainClass.methods, None)
        }
        extractClasses(classes) ++
          extractMethods(methods) ++
          extractStatements(stats)

      case None => extractClasses(cu.classes)
    })
  }

  private def extractClasses(newClasses: List[ClassDeclTree]): List[String] = {
    state.addClasses(newClasses)
    newClasses map { clazz =>
      Bold("Defined ") + KeywordColor("class ") + formatter.syntaxHighlight(clazz.tpe.toString)
    }
  }

  private def extractMethods(newMethods: List[MethodDeclTree]): List[String] = {
    state.addMethods(newMethods)
    newMethods map { meth =>
      Bold("Defined ") + KeywordColor("method ") + formatter.syntaxHighlight(meth.fullSignature)
    }
  }

  private def extractImports(imports: Imports): List[String] = {
    state.addImports(imports)
    imports.imports map { imp => Bold("Imported ") + formatter.syntaxHighlight(imp.writtenName) }
  }

  private def extractStatements(stat: Option[StatTree]): List[String] = {
    val stats = getStatements(stat)
    state.setNewStatements(stats)
    stats.filterInstance[VarDecl] map { variable =>
      val tpe = variable.tpe.map(t => ": " + t.name).getOrElse("")
      Bold("Defined ") + KeywordColor("variable ") + formatter.syntaxHighlight(variable.id.name + tpe)
    }
  }

  private def getStatements(stat: Option[StatTree]): List[StatTree] = {
    if (stat.isEmpty)
      return Nil

    stat.get match {
      case Block(stats) => stats
      case s            => s :: Nil
    }
  }

}
