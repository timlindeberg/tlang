package tlang.repl.terminal

import java.nio.charset.Charset

import com.googlecode.lanterna.TerminalSize
import com.googlecode.lanterna.TextColor.ANSI
import com.googlecode.lanterna.terminal.ansi.{UnixLikeTerminal, UnixTerminal}
import com.googlecode.lanterna.terminal.swing.TerminalEmulatorDeviceConfiguration.CursorStyle
import com.googlecode.lanterna.terminal.swing._
import com.googlecode.lanterna.terminal.{DefaultTerminalFactory, Terminal}
import tlang.utils.Extensions._

object TerminalFactory {

  def createTerminal(): Terminal = {
    if (sys.env.get("useTerminalEmulator").contains("true"))
      return createTerminalEmulator()

    val charset = Charset.forName(System.getProperty("file.encoding"))
    new UnixTerminal(System.in, System.out, charset, UnixLikeTerminal.CtrlCBehaviour.TRAP) use { term =>
      term.getInputDecoder.addProfile(CustomCharacterPatterns)
    }
  }

  private def createTerminalEmulator() =
    new DefaultTerminalFactory()
      .setTerminalEmulatorColorConfiguration(emulatorColors)
      .setTerminalEmulatorFontConfiguration(emulatorFont)
      .setInitialTerminalSize(new TerminalSize(80, 80))
      .setTerminalEmulatorDeviceConfiguration(deviceConfiguration)
      .createTerminal()

  private lazy val emulatorFont =
    new SwingTerminalFontConfiguration(
      true,
      AWTTerminalFontConfiguration.BoldMode.EVERYTHING,
      new java.awt.Font("Meslo LG S", 0, 14)
    )

  private lazy val deviceConfiguration =
    new TerminalEmulatorDeviceConfiguration(0, 200, CursorStyle.VERTICAL_BAR, ANSI.RED, true)

  private lazy val emulatorColors = TerminalEmulatorColorConfiguration.newInstance(new TerminalEmulatorPalette(
    new java.awt.Color(177, 204, 217), // defaultColor
    new java.awt.Color(177, 204, 217), // defaultBrightColor
    new java.awt.Color(50, 65, 72), //    defaultBackgroundColor
    new java.awt.Color(65, 87, 98), //    normalBlack
    new java.awt.Color(100, 133, 146), // brightBlack
    new java.awt.Color(247, 140, 108), // normalRed
    new java.awt.Color(255, 83, 112), //  brightRed
    new java.awt.Color(195, 232, 141), // normalGreen
    new java.awt.Color(204, 247, 175), // brightGreen
    new java.awt.Color(255, 203, 107), // normalYellow
    new java.awt.Color(255, 203, 67), //  brightYellow
    new java.awt.Color(130, 170, 255), // normalBlue
    new java.awt.Color(137, 221, 255), // brightBlue
    new java.awt.Color(199, 146, 234), // normalMagenta
    new java.awt.Color(207, 160, 237), // brightMagenta
    new java.awt.Color(147, 233, 217), // normalCyan
    new java.awt.Color(0, 185, 204), //   brightCyan
    new java.awt.Color(247, 247, 247), // normalWhite
    new java.awt.Color(255, 255, 255) //  brightWhite
  ))


}
