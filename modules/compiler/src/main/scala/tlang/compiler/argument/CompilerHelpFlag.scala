package tlang
package compiler
package argument

import tlang.options.argument.HelpFlag

case object CompilerHelpFlag extends HelpFlag(tlang.compiler.Main.CompilerFlags)
