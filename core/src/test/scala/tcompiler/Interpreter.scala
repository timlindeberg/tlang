
/*
 * Simplex3dConsole
 * Copyright (C) 2011, Aleksey Nikiforov
 *
 * This file is part of Simplex3dConsole.
 *
 * Simplex3dConsole is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Simplex3dConsole is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package tcompiler

import java.io.{PrintStream, PrintWriter, _}

import scala.tools.nsc.GenericRunnerSettings
import scala.tools.nsc.interpreter.IMain

class Interpreter {
  val baos          = new ByteArrayOutputStream()
  val stream        = new PrintStream(baos)
  val defaultStream = new PrintStream(new FileOutputStream(FileDescriptor.out))
  protected val flusher = new PrintWriter(stream)

  protected val interpreter: IMain = {
    val settings = new GenericRunnerSettings(System.out.println)
    settings.usejavacp.value = true
    settings.nc.value = true
    new IMain(settings, flusher)
  }

  def interpret(code: String): String = {
    System.setOut(stream)
    interpreter.interpret(code)
    flusher.flush()
    val res = baos.toString
    baos.reset()
    System.setOut(defaultStream)
    res
  }

  def reset() {
    interpreter.reset()
  }

  def dispose() {
    interpreter.close()
  }
}