package koolc

import utils._
import java.io.File
import lexer._
import ast._
import scala.collection.mutable.HashMap

object Main {

  val tokensFlag = "--tokens"
  val astFlag = "--ast"
  val flags = HashMap(tokensFlag -> false, astFlag -> false)

  def processOptions(args: Array[String]): Context = {

    val reporter = new Reporter()
    var outDir: Option[File] = None
    var files: List[File] = Nil

    def processOption(args: List[String]): Unit = args match {
      case "-d" :: out :: args =>
        outDir = Some(new File(out))
        processOption(args)

      case flag :: args if flags.contains(flag) =>
        flags(flag) = true
        processOption(args)

      case f :: args =>
        files = new File(f) :: files
        processOption(args)

      case Nil =>
    }

    processOption(args.toList)

    if (files.size != 1) {
      reporter.fatal("Exactly one file expected, " + files.size + " file(s) given.")
    }

    Context(reporter = reporter, file = files.head, outDir = outDir)
  }

  def main(args: Array[String]) {
    val ctx = processOptions(args)
    if (flags(tokensFlag)) {
      (Lexer andThen PrintTokens).run(ctx)(ctx.file).toList
    } else {
      val pipeline = Lexer andThen Parser
      val program = pipeline.run(ctx)(ctx.file)
      if (flags(astFlag)) {
        println(program)
      } else {
        println(Printer(program))
      }
    }
  }
}
