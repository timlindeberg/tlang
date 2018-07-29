package tlang
package repl
package argument

import tlang.options.argument.HelpFlag

case object ReplHelpFlag extends HelpFlag(tlang.repl.Main.ReplFlags)
