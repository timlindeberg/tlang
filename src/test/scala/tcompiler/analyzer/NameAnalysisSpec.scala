package tcompiler.analyzer

import java.io.File

import org.scalatest._
import tcompiler.TestUtils
import tcompiler.ast._
import tcompiler.lexer.Lexer
import tcompiler.utils.{CompilationException, Context}

import scala.collection.mutable.HashMap
import scala.sys.process._

class NameAnalysisSpec extends FlatSpec with Matchers with BeforeAndAfter {
  val flag = "--ast --symid"

  before {
    Symbols.ID.reset
  }

  behavior of "Positive tests"
  TestUtils.programFiles(TestUtils.Resources + "analyzer/name/valid/").foreach { file =>
    it should "name analyse program " + file.toPath() in test(file)
  }

  behavior of "Negative tests"
  TestUtils.programFiles(TestUtils.Resources + "analyzer/name/invalid/").foreach { file =>
    it should "name analyse program " + file.toPath() in test(file, true)
  }
  
  def test(file: File, exception: Boolean = false) = {
    val ctx = new Context(reporter = new tcompiler.utils.Reporter(exception), file = file, outDir = None)
    val exec = (Lexer andThen Parser andThen NameAnalysis).run(ctx)(_)
    if (exception) {
        intercept[CompilationException]{
          exec(ctx.file)  
        }
    } else {
      val program = exec(ctx.file)
      val res = replaceTypeIdentifiers(ASTPrinterWithSymbols(program))
      val correct = replaceIDNumbers(getAnswer(file), res)
      res + "\n" should be(correct)
    }
  }
  
  def replaceTypeIdentifiers(s: String): String =
    s.replaceAll("""TypeIdentifier#(\d+)\((.+?),List\(\)\)""", """Identifier#$1\($2\)""")
    
  def replaceIDNumbers(ast1: String, ast2: String): String = {
    val idRegex = """#(\d+)""".r
    def getSymbolIDs(ast: String) = idRegex.findAllIn(ast).matchData.map(_.group(1).toInt).toList
    def getSymbolIDMap(ast1: String, ast2: String) = {
      val map: HashMap[Int, Int] = HashMap()
      getSymbolIDs(ast1).zip(getSymbolIDs(ast2)).foreach {
        case (x, y) =>
          if (map.contains(x))
            assert(map(x) == y)
          map(x) = y
      }
      map
    }
    val idMap = getSymbolIDMap(ast1, ast2)
    idRegex.replaceAllIn(ast1, m => "#" + idMap(m.group(1).toInt))
  }

  def getAnswer(file: File) = Seq("", flag + " " + file.toPath()) !! TestUtils.IgnoreErrorOutput

}